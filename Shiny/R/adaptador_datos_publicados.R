resolver_ruta_publicada <- function(raiz_publicada, ruta_relativa,
    debe_existir = TRUE) {
  raiz <- normalizePath(raiz_publicada, winslash = "/", mustWork = TRUE)
  candidata <- normalizePath(file.path(raiz, ruta_relativa), winslash = "/",
    mustWork = debe_existir)
  prefijo <- paste0(tolower(raiz), "/")
  if (!identical(tolower(candidata), tolower(raiz)) &&
      !startsWith(tolower(candidata), prefijo)) {
    stop("El catalogo referencia una ruta fuera de data/published.",
      call. = FALSE)
  }
  candidata
}

leer_catalogo_publicado <- function(raiz_publicada) {
  archivos <- sort(list.files(
    file.path(raiz_publicada, "catalogo"),
    pattern = "^fecha=[0-9]{4}-[0-9]{2}-[0-9]{2}[.]yml$",
    full.names = TRUE
  ))
  if (!length(archivos)) {
    stop("No hay entradas en el catalogo publicado.", call. = FALSE)
  }
  entradas <- lapply(archivos, yaml::read_yaml)
  requeridas <- c(
    "fecha_operativa", "estado", "directorio", "directorio_referencias",
    "archivo_manifiesto", "directorio_ocupacion", "directorio_actividad",
    "directorio_viajes", "recorridos", "registros_viajes", "personas"
  )
  tablas <- lapply(seq_along(entradas), function(i) {
    entrada <- entradas[[i]]
    faltantes <- setdiff(requeridas, names(entrada))
    if (length(faltantes)) {
      stop(sprintf(
        "La entrada %s no contiene: %s.", basename(archivos[[i]]),
        paste(faltantes, collapse = ", ")
      ), call. = FALSE)
    }
    data.table::data.table(
      fecha_operativa = as.Date(as.character(entrada$fecha_operativa)),
      estado = as.character(entrada$estado),
      directorio = as.character(entrada$directorio),
      directorio_referencias = as.character(entrada$directorio_referencias),
      archivo_manifiesto = as.character(entrada$archivo_manifiesto),
      directorio_ocupacion = as.character(entrada$directorio_ocupacion),
      directorio_actividad = as.character(entrada$directorio_actividad),
      directorio_viajes = as.character(entrada$directorio_viajes),
      recorridos = as.integer(entrada$recorridos),
      registros_viajes = as.numeric(entrada$registros_viajes),
      personas = as.numeric(entrada$personas),
      archivo_catalogo = archivos[[i]]
    )
  })
  catalogo <- data.table::rbindlist(tablas, fill = TRUE)
  if (anyNA(catalogo$fecha_operativa) ||
      anyDuplicated(catalogo$fecha_operativa)) {
    stop("El catalogo publicado contiene fechas invalidas o duplicadas.",
      call. = FALSE)
  }
  catalogo <- catalogo[estado == "listo"]
  if (!nrow(catalogo)) {
    stop("El catalogo no contiene fechas con estado listo.", call. = FALSE)
  }
  data.table::setorder(catalogo, fecha_operativa)
  catalogo[]
}

leer_coleccion_particionada <- function(directorio) {
  archivos <- sort(list.files(
    directorio, pattern = "[.]parquet$", full.names = TRUE
  ))
  if (!length(archivos)) {
    stop(sprintf("La coleccion no contiene partes: %s.", directorio),
      call. = FALSE)
  }
  data.table::rbindlist(lapply(archivos, function(archivo) {
    data.table::as.data.table(arrow::read_parquet(archivo))
  }), use.names = TRUE, fill = FALSE)
}

descargar_objeto_publicado_a_cache <- function(
    cliente_s3, ubicacion, ruta_relativa, destino) {
  ruta_relativa <- gsub("\\\\", "/", ruta_relativa)
  metadatos <- obtener_metadatos_objeto_s3(
    cliente_s3, ruta_relativa,
    bucket = ubicacion$bucket, prefix = ubicacion$prefix
  )
  if (file.exists(destino) && !dir.exists(destino) &&
      identical(as.numeric(file.info(destino)$size), metadatos$bytes)) {
    etag_md5 <- grepl("^[a-fA-F0-9]{32}$", metadatos$etag)
    if (!etag_md5 || identical(
        tolower(unname(tools::md5sum(destino))), tolower(metadatos$etag))) {
      return(destino)
    }
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- tempfile(".objeto_s3_", tmpdir = dirname(destino))
  on.exit(unlink(temporal, force = TRUE), add = TRUE)
  descargar_archivo_s3(
    cliente_s3, ruta_relativa, temporal,
    bucket = ubicacion$bucket, prefix = ubicacion$prefix
  )
  if (!identical(as.numeric(file.info(temporal)$size), metadatos$bytes)) {
    stop(sprintf("La descarga S3 quedo incompleta: %s.", ruta_relativa),
      call. = FALSE)
  }
  if (file.exists(destino)) unlink(destino, force = TRUE)
  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover al cache: %s.", destino),
      call. = FALSE)
  }
  destino
}

validar_inventario_publicado <- function(directorio) {
  archivo_inventario <- file.path(directorio, "manifiesto_archivos.csv")
  if (!file.exists(archivo_inventario)) {
    stop("La publicacion materializada no contiene manifiesto de archivos.",
      call. = FALSE)
  }
  inventario <- data.table::fread(archivo_inventario)
  requeridas <- c("archivo", "bytes", "md5")
  if (!all(requeridas %in% names(inventario)) || !nrow(inventario)) {
    stop("El manifiesto de archivos publicado esta incompleto.",
      call. = FALSE)
  }
  for (i in seq_len(nrow(inventario))) {
    ruta <- ruta_local_relativa(
      directorio, as.character(inventario$archivo[[i]])
    )
    valido <- file.exists(ruta) && !dir.exists(ruta) &&
      identical(
        as.numeric(file.info(ruta)$size), as.numeric(inventario$bytes[[i]])
      ) && identical(
        unname(tools::md5sum(ruta)), as.character(inventario$md5[[i]])
      )
    if (!valido) {
      stop(sprintf("Archivo publicado ausente o invalido: %s.",
        inventario$archivo[[i]]), call. = FALSE)
    }
  }
  invisible(inventario)
}

materializar_base_publicada_s3 <- function(
    raiz_publicada,
    cache_root,
    version_presentacion,
    cliente_s3) {
  ubicacion <- descomponer_uri_s3(raiz_publicada)
  cache_publicado <- file.path(cache_root, "shiny", "published")
  dir.create(cache_publicado, recursive = TRUE, showWarnings = FALSE)
  sincronizar_prefijo_s3(
    cliente_s3, "catalogo", file.path(cache_publicado, "catalogo"),
    bucket = ubicacion$bucket, prefix = ubicacion$prefix
  )
  catalogo <- leer_catalogo_publicado(cache_publicado)

  manifiestos <- vector("list", nrow(catalogo))
  for (i in seq_len(nrow(catalogo))) {
    relativa <- unir_key_s3(
      catalogo$directorio[[i]], catalogo$archivo_manifiesto[[i]]
    )
    destino <- resolver_ruta_publicada(
      cache_publicado, relativa, debe_existir = FALSE
    )
    descargar_objeto_publicado_a_cache(
      cliente_s3, ubicacion, relativa, destino
    )
    manifiestos[[i]] <- yaml::read_yaml(destino)
  }
  referencias <- unique(catalogo$directorio_referencias)
  for (relativa in referencias) {
    destino <- resolver_ruta_publicada(
      cache_publicado, relativa, debe_existir = FALSE
    )
    descargar_prefijo_s3(
      cliente_s3, gsub("\\\\", "/", relativa), destino,
      bucket = ubicacion$bucket, prefix = ubicacion$prefix,
      reutilizar_existente = TRUE
    )
  }
  redes <- unique(vapply(
    manifiestos, function(x) as.character(x$red_canonica), character(1L)
  ))
  for (red in redes) {
    relativa <- unir_key_s3(
      "referencias", paste0("red=", red),
      paste0("presentacion=", version_presentacion)
    )
    destino <- resolver_ruta_publicada(
      cache_publicado, relativa, debe_existir = FALSE
    )
    descargar_prefijo_s3(
      cliente_s3, relativa, destino,
      bucket = ubicacion$bucket, prefix = ubicacion$prefix,
      reutilizar_existente = TRUE
    )
  }
  datos <- cargar_base_shiny_publicada(
    cache_publicado, version_presentacion = version_presentacion
  )
  datos$published_backend <- "s3"
  datos$remote_published_root <- sub("/+$", "", raiz_publicada)
  datos$s3_client <- cliente_s3
  datos$s3_location <- ubicacion
  datos
}

materializar_dia_publicado_s3 <- function(datos, fila) {
  relativa <- gsub("\\\\", "/", fila$directorio[[1L]])
  destino <- resolver_ruta_publicada(
    datos$published_root, relativa, debe_existir = FALSE
  )
  sincronizar_prefijo_s3(
    datos$s3_client, relativa, destino,
    bucket = datos$s3_location$bucket,
    prefix = datos$s3_location$prefix
  )
  validar_inventario_publicado(destino)
  destino
}

renombrar_columna_sf <- function(x, origen, destino) {
  if (origen %in% names(x)) names(x)[names(x) == origen] <- destino
  x
}

adaptar_formas_regionales <- function(formas) {
  requeridas <- c("barrios", "ccz", "municipios", "departamentos")
  if (!all(requeridas %in% names(formas))) {
    stop("Las formas regionales publicadas estan incompletas.", call. = FALSE)
  }
  list(
    barrios = renombrar_columna_sf(formas$barrios, "barrio", "NOMBBARR"),
    ccz = renombrar_columna_sf(formas$ccz, "ccz", "CCZ"),
    municipio = renombrar_columna_sf(
      formas$municipios, "municipio", "MUNICIPIO"
    ),
    departamento = renombrar_columna_sf(
      formas$departamentos, "departamento", "DEPARTAMENTO"
    )
  )
}

validar_compatibilidad_referencias <- function(
    raiz_publicada, catalogo, version_presentacion) {
  manifiestos <- lapply(seq_len(nrow(catalogo)), function(i) {
    directorio_dia <- resolver_ruta_publicada(
      raiz_publicada, catalogo$directorio[[i]]
    )
    yaml::read_yaml(file.path(
      directorio_dia, catalogo$archivo_manifiesto[[i]]
    ))
  })
  claves <- data.table::rbindlist(lapply(manifiestos, function(x) {
    data.table::data.table(
      red_canonica = as.character(x$red_canonica),
      version_registro_variantes =
        as.character(x$version_registro_variantes),
      snapshot_referencias = as.character(x$snapshot_referencias)
    )
  }))
  fechas_manifiesto <- as.Date(vapply(
    manifiestos, function(x) as.character(x$fecha_operativa), character(1L)
  ))
  controles_fallidos <- vapply(
    manifiestos, function(x) as.integer(x$controles_fallidos), integer(1L)
  )
  if (!identical(fechas_manifiesto, catalogo$fecha_operativa) ||
      anyNA(controles_fallidos) || any(controles_fallidos != 0L)) {
    stop("Los manifiestos diarios no coinciden con el catalogo listo.",
      call. = FALSE)
  }
  if (nrow(unique(claves)) != 1L ||
      data.table::uniqueN(catalogo$directorio_referencias) != 1L) {
    stop(paste(
      "El adaptador inicial requiere que las fechas listas compartan",
      "red, variantes, snapshot y registro regional."
    ), call. = FALSE)
  }
  clave <- claves[1L]
  relativo_presentacion <- file.path(
    "referencias", paste0("red=", clave$red_canonica),
    paste0("presentacion=", version_presentacion)
  )
  directorio_presentacion <- resolver_ruta_publicada(
    raiz_publicada, relativo_presentacion
  )
  manifiesto_presentacion <- yaml::read_yaml(file.path(
    directorio_presentacion, "manifiesto_publicacion.yml"
  ))
  compatible <- identical(
    as.character(manifiesto_presentacion$estado), "listo"
  ) && identical(
    as.character(manifiesto_presentacion$red_canonica),
    clave$red_canonica
  ) && identical(
    as.character(manifiesto_presentacion$version_registro_variantes),
    clave$version_registro_variantes
  ) && identical(
    as.character(manifiesto_presentacion$snapshot_referencias),
    clave$snapshot_referencias
  )
  if (!compatible) {
    stop("La referencia de presentacion no corresponde a los dias publicados.",
      call. = FALSE)
  }
  list(
    clave = clave,
    manifiestos_dia = manifiestos,
    directorio_presentacion = directorio_presentacion,
    directorio_regiones = resolver_ruta_publicada(
      raiz_publicada, catalogo$directorio_referencias[[1L]]
    )
  )
}

adaptar_presentacion_publicada <- function(tabla) {
  ocurrencias <- unique(tabla[, .(
    id_version_variante,
    indice_secuencia,
    punto_canonico_id,
    COD_VARIAN = as.character(codigo_variante),
    COD_LINEA = as.character(codigo_linea),
    seqViajeN = as.integer(indice_secuencia),
    ID_punto = as.character(punto_canonico_id),
    ID_presentacion = as.character(id_presentacion),
    COD_NOMBRE = as.character(corredor_id),
    NOM_CALLE = as.character(nombre_via),
    street_key = as.character(clave_via),
    network_zone = as.character(zona_red),
    dirAv = as.character(sentido_recorrido)
  )])
  if (anyDuplicated(ocurrencias[, .(
      id_version_variante, indice_secuencia, punto_canonico_id
  )])) {
    stop("La referencia publicada duplica ocurrencias canonicas.",
      call. = FALSE)
  }
  presentacion <- unique(tabla[, .(
    ID_presentacion = as.character(id_presentacion),
    ID_punto = as.character(punto_canonico_id),
    COD_NOMBRE = as.character(corredor_id),
    NOM_CALLE = as.character(nombre_via),
    street_key = as.character(clave_via),
    network_zone = as.character(zona_red),
    dirAv = as.character(sentido_recorrido),
    X_mapa = as.numeric(x_mapa),
    Y_mapa = as.numeric(y_mapa)
  )], by = "ID_presentacion")
  geo <- sf::st_as_sf(
    as.data.frame(presentacion), coords = c("X_mapa", "Y_mapa"),
    crs = 32721, remove = FALSE
  )
  geo <- sf::st_transform(geo, 4326)
  xy <- sf::st_coordinates(geo)
  presentacion[, `:=`(lon = xy[, 1], lat = xy[, 2])]
  puntos <- unique(tabla[, .(
    ID_punto = as.character(punto_canonico_id),
    X = as.numeric(x_canonica), Y = as.numeric(y_canonica),
    NOM_CALLE = as.character(nombre_via),
    dirAv = as.character(sentido_recorrido)
  )], by = "ID_punto")
  variantes_detalle <- unique(tabla[, .(
    COD_VARIAN = as.character(codigo_variante),
    COD_LINEA = as.character(codigo_linea),
    DESC_LINEA = as.character(nombre_linea)
  )])
  if (nrow(variantes_detalle[, .N, by = COD_VARIAN][N != 1L])) {
    stop("La presentacion define metadatos ambiguos por variante.",
      call. = FALSE)
  }
  variantes <- variantes_detalle
  list(
    occurrences = ocurrencias,
    presentation_points = presentacion,
    points = puntos,
    variants = variantes,
    lines = unique(variantes[, .(COD_LINEA, DESC_LINEA)])[order(DESC_LINEA)]
  )
}

cargar_base_shiny_publicada <- function(
    raiz_publicada,
    version_presentacion = "presentacion_202605_v1") {
  for (paquete in c("arrow", "data.table", "sf", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete),
        call. = FALSE)
    }
  }
  catalogo <- leer_catalogo_publicado(raiz_publicada)
  referencias <- validar_compatibilidad_referencias(
    raiz_publicada, catalogo, version_presentacion
  )
  tabla_presentacion <- data.table::as.data.table(arrow::read_parquet(
    file.path(
      referencias$directorio_presentacion,
      "secuencias_presentacion.parquet"
    )
  ))
  presentacion <- adaptar_presentacion_publicada(tabla_presentacion)
  vias <- data.table::as.data.table(arrow::read_parquet(file.path(
    referencias$directorio_presentacion, "catalogo_vias.parquet"
  )))
  regiones <- data.table::as.data.table(arrow::read_parquet(file.path(
    referencias$directorio_regiones, "paradas_regiones.parquet"
  )))
  formas <- adaptar_formas_regionales(readRDS(file.path(
    referencias$directorio_regiones, "formas_regiones.rds"
  )))
  paradas <- unique(regiones[, .(
    COD_UBIC_P = as.character(codigo_parada),
    CALLE = as.character(nombre_parada),
    lon = as.numeric(longitud), lat = as.numeric(latitud)
  )], by = "COD_UBIC_P")
  regiones_compatibles <- unique(regiones[, .(
    COD_UBIC_P = as.character(codigo_parada),
    BARRIO = as.character(barrio), CCZ = as.character(ccz),
    MUNICIPIO = as.character(municipio),
    DEPARTAMENTO = as.character(departamento)
  )], by = "COD_UBIC_P")
  primera <- catalogo[1L]
  calidad <- data.table::data.table(
    estado = rep("horario_completo", primera$recorridos)
  )
  list(
    bus = data.table::data.table(),
    stop_passes = data.table::data.table(),
    trips = data.table::data.table(),
    points = presentacion$points,
    presentation_points = presentacion$presentation_points,
    occurrences = presentacion$occurrences,
    variants = presentacion$variants,
    lines = presentacion$lines,
    dates = catalogo$fecha_operativa,
    quality = calidad,
    stops = paradas,
    stop_regions = regiones_compatibles,
    shapes = formas,
    zone_choices = list(
      barrio = sort(unique(stats::na.omit(regiones_compatibles$BARRIO))),
      ccz = sort(unique(stats::na.omit(regiones_compatibles$CCZ))),
      municipio = sort(unique(stats::na.omit(
        regiones_compatibles$MUNICIPIO
      ))),
      departamento = sort(unique(stats::na.omit(
        regiones_compatibles$DEPARTAMENTO
      )))
    ),
    street_choices = vias[, .(
      street_key = as.character(clave_via),
      street_label = as.character(etiqueta_via)
    )],
    profile = "publicado_limpio",
    source_label = "dias operativos del pipeline limpio",
    daily_catalog = catalogo,
    daily_catalog_path = file.path(raiz_publicada, "catalogo"),
    published_root = normalizePath(
      raiz_publicada, winslash = "/", mustWork = TRUE
    ),
    published_backend = "local",
    daily_cache = new.env(parent = emptyenv())
  )
}

adaptar_ocupacion_publicada <- function(tabla, ocurrencias) {
  filas <- nrow(tabla)
  resultado <- merge(
    tabla, ocurrencias,
    by = c("id_version_variante", "indice_secuencia", "punto_canonico_id"),
    all.x = TRUE, sort = FALSE
  )
  if (nrow(resultado) != filas || anyNA(resultado$ID_presentacion)) {
    stop("La ocupacion publicada no resuelve en la presentacion.",
      call. = FALSE)
  }
  resultado[, .(
    recorrido = as.character(recorrido),
    id_version_variante = as.character(id_version_variante),
    COD_VARIAN, COD_LINEA, seqViajeN, ID_punto, ID_presentacion,
    COD_NOMBRE, NOM_CALLE, street_key, network_zone, dirAv,
    tiempo, buses = as.numeric(buses),
    ocupacion_personas = as.numeric(ocupacion_personas),
    metodo_tiempo = as.character(metodo_tiempo),
    tipo_servicio = as.character(tipo_servicio)
  )]
}

adaptar_actividad_publicada <- function(tabla) {
  tabla[, .(
    fecha_operativa = as.Date(fecha_operativa),
    recorrido = as.character(recorrido),
    COD_VARIAN = as.character(codigo_variante),
    COD_LINEA = as.character(codigo_linea),
    seqViajeN = as.integer(indice_secuencia),
    ID_punto = as.character(punto_canonico_id),
    COD_UBIC_P = as.character(codigo_parada),
    tiempo, buses = as.numeric(buses), subidas = as.numeric(subidas),
    bajadas = as.numeric(bajadas),
    ocupacion_personas = as.numeric(ocupacion_personas),
    metodo_tiempo = as.character(metodo_tiempo),
    tipo_servicio = as.character(tipo_servicio)
  )]
}

adaptar_viajes_publicados <- function(tabla) {
  tabla[, .(
    fecha_operativa = as.Date(fecha_operativa),
    trip_id,
    recorrido = as.character(recorrido),
    COD_VARIAN = as.character(codigo_variante),
    COD_LINEA = as.character(codigo_linea),
    seq_origen_expansion = as.integer(indice_secuencia_origen),
    seq_destino_expansion = as.integer(indice_secuencia_destino),
    COD_PARADA_ORIGEN_EXPANSION = as.character(codigo_parada_origen),
    COD_PARADA_DESTINO_EXPANSION = as.character(codigo_parada_destino),
    tiempo_subida = tiempo_estimado_subida,
    tiempo_bajada = tiempo_estimado_bajada,
    cantidad_pasajeros = as.numeric(cantidad_pasajeros),
    tipo_servicio = as.character(tipo_servicio),
    recuperada_paso10 = as.logical(recuperada_paso10),
    requiere_revision = as.logical(requiere_revision)
  )]
}

cargar_dia_shiny_publicado <- function(datos, fecha) {
  clave_cache <- as.character(as.Date(fecha))
  if (exists(clave_cache, envir = datos$daily_cache, inherits = FALSE)) {
    return(get(clave_cache, envir = datos$daily_cache, inherits = FALSE))
  }
  fila <- datos$daily_catalog[fecha_operativa == as.Date(fecha)]
  if (nrow(fila) != 1L) {
    stop(sprintf("La fecha %s no esta publicada y lista.", fecha),
      call. = FALSE)
  }
  directorio_dia <- if (identical(datos$published_backend, "s3")) {
    materializar_dia_publicado_s3(datos, fila)
  } else {
    resolver_ruta_publicada(datos$published_root, fila$directorio[[1L]])
  }
  bus_limpio <- leer_coleccion_particionada(file.path(
    directorio_dia, fila$directorio_ocupacion[[1L]]
  ))
  actividad_limpia <- leer_coleccion_particionada(file.path(
    directorio_dia, fila$directorio_actividad[[1L]]
  ))
  viajes_limpios <- leer_coleccion_particionada(file.path(
    directorio_dia, fila$directorio_viajes[[1L]]
  ))
  manifiesto_dia <- yaml::read_yaml(file.path(
    directorio_dia, fila$archivo_manifiesto[[1L]]
  ))
  if (data.table::uniqueN(bus_limpio$recorrido) != fila$recorridos ||
      nrow(viajes_limpios) != fila$registros_viajes ||
      sum(viajes_limpios$cantidad_pasajeros) != fila$personas ||
      nrow(bus_limpio) != as.numeric(manifiesto_dia$filas_bus) ||
      nrow(actividad_limpia) != as.numeric(manifiesto_dia$filas_actividad)) {
    stop("Las colecciones diarias no coinciden con el catalogo.",
      call. = FALSE)
  }
  resultado <- datos
  resultado$bus <- adaptar_ocupacion_publicada(
    bus_limpio, datos$occurrences
  )
  resultado$stop_passes <- adaptar_actividad_publicada(actividad_limpia)
  resultado$trips <- adaptar_viajes_publicados(viajes_limpios)
  assign(clave_cache, resultado, envir = datos$daily_cache)
  resultado
}
