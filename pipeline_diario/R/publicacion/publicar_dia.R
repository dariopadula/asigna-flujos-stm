publicar_referencias_estaticas <- function(
    directorio_regiones,
    directorio_red_canonica,
    directorio_publicado,
    red_canonica,
    version_regiones) {
  fuentes <- c(
    paradas_regiones = file.path(
      directorio_regiones, "paradas_regiones.parquet"
    ),
    formas_regiones = file.path(
      directorio_regiones, "formas_regiones.rds"
    ),
    puntos_canonicos = file.path(
      directorio_red_canonica, "catalogo_puntos.rds"
    ),
    corredores_canonicos = file.path(
      directorio_red_canonica, "catalogo_corredores.rds"
    )
  )
  faltantes <- fuentes[!file.exists(fuentes)]
  if (length(faltantes)) {
    stop(sprintf(
      "Faltan referencias estaticas para publicar: %s",
      paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }
  relativo <- file.path(
    "referencias", paste0("red=", red_canonica),
    paste0("regiones=", version_regiones)
  )
  destino <- file.path(directorio_publicado, relativo)
  if (dir.exists(destino)) {
    inventario <- file.path(destino, "manifiesto_archivos.csv")
    if (!file.exists(inventario)) {
      stop("Las referencias publicadas estan incompletas.", call. = FALSE)
    }
    registrado <- data.table::fread(inventario)
    nombres <- basename(fuentes)
    esperado <- unname(tools::md5sum(fuentes))
    observado <- registrado[match(nombres, archivo), md5]
    if (anyNA(observado) || !identical(observado, esperado)) {
      stop("Las referencias publicadas no coinciden con el registro.",
        call. = FALSE)
    }
    return(relativo)
  }
  temporal <- paste0(destino, ".preparando")
  if (dir.exists(temporal)) {
    stop("Hay una publicacion estatica incompleta.", call. = FALSE)
  }
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  destinos <- file.path(temporal, basename(fuentes))
  copiados <- file.copy(fuentes, destinos, overwrite = FALSE)
  if (!all(copiados)) {
    stop("No se pudieron copiar las referencias estaticas.", call. = FALSE)
  }
  inventario <- data.table::data.table(
    archivo = basename(destinos),
    bytes = as.numeric(file.info(destinos)$size),
    md5 = unname(tools::md5sum(destinos))
  )
  data.table::fwrite(
    inventario, file.path(temporal, "manifiesto_archivos.csv")
  )
  yaml::write_yaml(list(
    tipo = "referencias_consulta_publicadas",
    red_canonica = red_canonica,
    version_regiones = version_regiones,
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))
  if (!file.rename(temporal, destino)) {
    stop("No se pudieron promover las referencias estaticas.", call. = FALSE)
  }
  relativo
}

registrar_dia_publicado <- function(
    directorio_publicado, fecha, entrada) {
  directorio_catalogo <- file.path(directorio_publicado, "catalogo")
  dir.create(directorio_catalogo, recursive = TRUE, showWarnings = FALSE)
  destino <- file.path(
    directorio_catalogo, paste0("fecha=", fecha, ".yml")
  )
  temporal <- paste0(destino, ".tmp")
  if (file.exists(destino) || file.exists(temporal)) {
    stop(sprintf("El catalogo ya contiene la fecha %s.", fecha),
      call. = FALSE)
  }
  yaml::write_yaml(entrada, temporal)
  if (!file.rename(temporal, destino)) {
    stop("No se pudo registrar atomicamente la fecha publicada.",
      call. = FALSE)
  }
  destino
}

publicar_dia <- function(
    fecha,
    directorio_ocupacion,
    directorio_insumos,
    directorio_regiones,
    directorio_red_canonica,
    directorio_publicado) {
  for (paquete in c("arrow", "data.table", "tidyselect", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete),
        call. = FALSE)
    }
  }
  fecha_date <- as.Date(fecha)
  if (length(fecha) != 1L || is.na(fecha_date) ||
      format(fecha_date, "%Y-%m-%d") != fecha) {
    stop("La fecha debe tener formato AAAA-MM-DD.", call. = FALSE)
  }
  manifiestos_archivo <- c(
    ocupacion = file.path(directorio_ocupacion, "manifiesto.yml"),
    insumos = file.path(directorio_insumos, "manifiesto.yml"),
    regiones = file.path(directorio_regiones, "manifiesto.yml"),
    red = file.path(directorio_red_canonica, "manifiesto.yml")
  )
  archivo_regiones <- file.path(
    directorio_regiones, "paradas_regiones.parquet"
  )
  entradas <- c(manifiestos_archivo, archivo_regiones)
  faltantes <- entradas[!file.exists(entradas)]
  if (length(faltantes)) {
    stop(sprintf(
      "Faltan entradas para publicar el dia: %s",
      paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }
  manifiestos <- lapply(manifiestos_archivo, yaml::read_yaml)
  metadatos_coinciden <-
    identical(as.character(manifiestos$ocupacion$fecha_operativa), fecha) &&
    identical(as.character(manifiestos$insumos$fecha_operativa), fecha) &&
    identical(
      as.character(manifiestos$ocupacion$version_registro_variantes),
      as.character(manifiestos$insumos$version_registro_variantes)
    ) &&
    identical(
      as.character(manifiestos$ocupacion$version_registro_variantes),
      as.character(manifiestos$regiones$version_registro_variantes)
    ) &&
    identical(
      as.character(manifiestos$ocupacion$snapshot_referencias),
      as.character(manifiestos$insumos$snapshot_referencias)
    ) &&
    identical(
      as.character(manifiestos$ocupacion$snapshot_referencias),
      as.character(manifiestos$regiones$snapshot_referencias)
    ) &&
    identical(
      as.character(manifiestos$ocupacion$red_canonica),
      as.character(manifiestos$insumos$red_canonica)
    ) &&
    identical(
      as.character(manifiestos$ocupacion$red_canonica),
      as.character(manifiestos$regiones$red_canonica)
    ) &&
    identical(
      as.character(manifiestos$ocupacion$red_canonica),
      as.character(manifiestos$red$version_red)
    )
  entradas_aprobadas <- all(c(
    as.integer(manifiestos$ocupacion$controles_fallidos),
    as.integer(manifiestos$insumos$controles_fallidos),
    as.integer(manifiestos$regiones$controles_fallidos)
  ) == 0L)
  if (!metadatos_coinciden || !entradas_aprobadas) {
    stop("Las entradas no estan aprobadas o no pertenecen a la misma entrega.",
      call. = FALSE)
  }

  partes_bus <- sort(list.files(
    file.path(directorio_ocupacion, "pasadas_ocupacion"),
    pattern = "[.]parquet$", full.names = TRUE
  ))
  partes_actividad <- sort(list.files(
    file.path(directorio_insumos, "actividad_paradas"),
    pattern = "[.]parquet$", full.names = TRUE
  ))
  partes_viajes <- sort(list.files(
    file.path(directorio_insumos, "viajes_consulta"),
    pattern = "[.]parquet$", full.names = TRUE
  ))
  particiones_coinciden <- length(partes_bus) > 0L &&
    identical(basename(partes_bus), basename(partes_actividad)) &&
    identical(basename(partes_bus), basename(partes_viajes))
  if (!particiones_coinciden) {
    stop("Las particiones de ocupacion e insumos no coinciden.",
      call. = FALSE)
  }

  regiones <- data.table::as.data.table(arrow::read_parquet(
    archivo_regiones
  ))
  regiones[, codigo_parada := trimws(as.character(codigo_parada))]
  if (anyNA(regiones$codigo_parada) ||
      anyDuplicated(regiones$codigo_parada)) {
    stop("El registro regional no tiene una fila unica por parada.",
      call. = FALSE)
  }
  columnas_region <- c("barrio", "ccz", "municipio", "departamento")
  if (!all(columnas_region %in% names(regiones))) {
    stop("El registro regional esta incompleto.", call. = FALSE)
  }
  indice_region <- regiones[, c("codigo_parada", columnas_region), with = FALSE]
  data.table::setkey(indice_region, codigo_parada)

  relativo_dia <- file.path("dias", paste0("fecha=", fecha))
  destino <- file.path(directorio_publicado, relativo_dia)
  temporal <- file.path(
    directorio_publicado, "dias", paste0(".preparando_fecha=", fecha)
  )
  entrada_catalogo <- file.path(
    directorio_publicado, "catalogo", paste0("fecha=", fecha, ".yml")
  )
  if (dir.exists(destino) || dir.exists(temporal) ||
      file.exists(entrada_catalogo)) {
    stop(sprintf("La fecha %s ya existe o quedo incompleta.", fecha),
      call. = FALSE)
  }
  dir.create(file.path(temporal, "pasadas_ocupacion"),
    recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(temporal, "actividad_paradas"),
    recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(temporal, "viajes_consulta"),
    recursive = TRUE, showWarnings = FALSE)

  filas_bus <- filas_actividad <- filas_viajes <- 0
  personas <- subidas <- bajadas <- pasadas_bus <- 0
  recorridos <- vector("list", length(partes_bus))
  ids_viaje <- vector("list", length(partes_bus))
  fechas_validas <- servicios_validos <- TRUE
  regiones_actividad_completas <- regiones_origen_completas <-
    regiones_destino_completas <- TRUE
  for (i in seq_along(partes_bus)) {
    nombre <- basename(partes_bus[[i]])
    destino_bus <- file.path(temporal, "pasadas_ocupacion", nombre)
    if (!file.copy(partes_bus[[i]], destino_bus, overwrite = FALSE)) {
      stop("No se pudo copiar una particion de ocupacion.", call. = FALSE)
    }
    bus <- data.table::as.data.table(arrow::read_parquet(
      partes_bus[[i]],
      col_select = tidyselect::all_of(c(
        "recorrido", "tipo_servicio"
      ))
    ))
    actividad <- data.table::as.data.table(arrow::read_parquet(
      partes_actividad[[i]]
    ))
    viajes <- data.table::as.data.table(arrow::read_parquet(
      partes_viajes[[i]]
    ))
    actividad[, codigo_parada := trimws(as.character(codigo_parada))]
    viajes[, `:=`(
      codigo_parada_origen = trimws(as.character(codigo_parada_origen)),
      codigo_parada_destino = trimws(as.character(codigo_parada_destino))
    )]
    regiones_actividad_completas <- regiones_actividad_completas &&
      all(actividad$codigo_parada %in% indice_region$codigo_parada)
    regiones_origen_completas <- regiones_origen_completas &&
      all(viajes$codigo_parada_origen %in% indice_region$codigo_parada)
    regiones_destino_completas <- regiones_destino_completas &&
      all(viajes$codigo_parada_destino %in% indice_region$codigo_parada)
    actividad <- merge(
      actividad, indice_region,
      by = "codigo_parada", all.x = TRUE, sort = FALSE
    )
    origen <- data.table::copy(indice_region)
    data.table::setnames(
      origen,
      c("codigo_parada", columnas_region),
      c("codigo_parada_origen", paste0(columnas_region, "_origen"))
    )
    destino_region <- data.table::copy(indice_region)
    data.table::setnames(
      destino_region,
      c("codigo_parada", columnas_region),
      c("codigo_parada_destino", paste0(columnas_region, "_destino"))
    )
    viajes <- merge(
      viajes, origen, by = "codigo_parada_origen",
      all.x = TRUE, sort = FALSE
    )
    viajes <- merge(
      viajes, destino_region, by = "codigo_parada_destino",
      all.x = TRUE, sort = FALSE
    )
    arrow::write_parquet(
      actividad, file.path(temporal, "actividad_paradas", nombre),
      compression = "zstd"
    )
    arrow::write_parquet(
      viajes, file.path(temporal, "viajes_consulta", nombre),
      compression = "zstd"
    )
    filas_bus <- filas_bus + nrow(bus)
    filas_actividad <- filas_actividad + nrow(actividad)
    filas_viajes <- filas_viajes + nrow(viajes)
    personas <- personas + sum(as.numeric(viajes$cantidad_pasajeros))
    subidas <- subidas + sum(as.numeric(actividad$subidas))
    bajadas <- bajadas + sum(as.numeric(actividad$bajadas))
    pasadas_bus <- pasadas_bus + sum(as.numeric(actividad$buses))
    recorridos[[i]] <- unique(bus$recorrido)
    ids_viaje[[i]] <- viajes$trip_id
    fechas_validas <- fechas_validas &&
      all(substr(bus$recorrido, 1L, 10L) == fecha) &&
      all(as.Date(actividad$fecha_operativa) == fecha_date) &&
      all(as.Date(viajes$fecha_operativa) == fecha_date)
    servicios_validos <- servicios_validos &&
      all(bus$tipo_servicio %in% c("urbano", "inter")) &&
      all(actividad$tipo_servicio %in% c("urbano", "inter")) &&
      all(viajes$tipo_servicio %in% c("urbano", "inter"))
  }
  recorridos <- unique(unlist(recorridos, use.names = FALSE))
  ids_viaje <- unlist(ids_viaje, use.names = FALSE)
  bus_copiado <- sort(list.files(
    file.path(temporal, "pasadas_ocupacion"),
    pattern = "[.]parquet$", full.names = TRUE
  ))
  copia_bus_exacta <- identical(
    unname(tools::md5sum(partes_bus)),
    unname(tools::md5sum(bus_copiado))
  )

  controles <- data.table::data.table(
    control = c(
      "metadatos_coinciden", "entradas_aprobadas",
      "particiones_coinciden", "ocupacion_copiada_exactamente",
      "fechas_operativas_coinciden", "servicios_validos",
      "regiones_cubren_actividad", "regiones_cubren_origenes",
      "regiones_cubren_destinos", "actividad_conserva_filas",
      "viajes_conservan_filas", "viajes_unicos",
      "recorridos_coinciden", "personas_coinciden",
      "subidas_coinciden", "bajadas_coinciden",
      "personas_igual_subidas", "subidas_igual_bajadas",
      "pasadas_actividad_coinciden", "ocupacion_conserva_filas",
      "cantidad_lotes_coincide",
      "actividad_regionalizada", "viajes_regionalizados"
    ),
    aprobado = c(
      metadatos_coinciden,
      entradas_aprobadas,
      particiones_coinciden,
      copia_bus_exacta,
      fechas_validas,
      servicios_validos,
      regiones_actividad_completas,
      regiones_origen_completas,
      regiones_destino_completas,
      filas_actividad == as.numeric(manifiestos$insumos$filas_actividad),
      filas_viajes == as.numeric(manifiestos$insumos$registros_viajes),
      length(ids_viaje) == length(unique(ids_viaje)),
      length(recorridos) == as.integer(manifiestos$ocupacion$recorridos),
      personas == as.numeric(manifiestos$insumos$personas),
      subidas == as.numeric(manifiestos$insumos$subidas),
      bajadas == as.numeric(manifiestos$insumos$bajadas),
      personas == subidas,
      subidas == bajadas,
      pasadas_bus == as.numeric(manifiestos$insumos$pasadas_bus),
      filas_bus == as.numeric(manifiestos$ocupacion$filas_pasadas),
      length(partes_bus) == as.integer(manifiestos$insumos$cantidad_lotes),
      regiones_actividad_completas,
      regiones_origen_completas && regiones_destino_completas
    )
  )
  controles[, estado := data.table::fifelse(
    aprobado, "aprobado", "fallo"
  )]
  if (any(!controles$aprobado)) {
    stop("La fecha no aprobo los controles previos a publicacion.",
      call. = FALSE)
  }
  resumen <- data.table::data.table(
    fecha_operativa = fecha,
    recorridos = length(recorridos),
    filas_bus = filas_bus,
    filas_actividad = filas_actividad,
    registros_viajes = filas_viajes,
    personas = personas,
    subidas = subidas,
    bajadas = bajadas,
    pasadas_bus = pasadas_bus
  )
  data.table::fwrite(resumen, file.path(temporal, "resumen_dia.csv"))
  data.table::fwrite(controles, file.path(temporal, "controles.csv"))
  archivos_dia <- list.files(
    temporal, recursive = TRUE, full.names = TRUE
  )
  inventario <- data.table::data.table(
    archivo = substring(
      normalizePath(archivos_dia, winslash = "/", mustWork = TRUE),
      nchar(normalizePath(temporal, winslash = "/", mustWork = TRUE)) + 2L
    ),
    bytes = as.numeric(file.info(archivos_dia)$size),
    md5 = unname(tools::md5sum(archivos_dia))
  )
  data.table::fwrite(
    inventario, file.path(temporal, "manifiesto_archivos.csv")
  )
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    estado = "listo",
    tipo_salida = "publicacion_diaria_regionalizada",
    version_registro_variantes =
      as.character(manifiestos$insumos$version_registro_variantes),
    snapshot_referencias =
      as.character(manifiestos$insumos$snapshot_referencias),
    red_canonica = as.character(manifiestos$insumos$red_canonica),
    version_regiones = as.character(manifiestos$regiones$version_regiones),
    cantidad_lotes = length(partes_bus),
    recorridos = length(recorridos),
    filas_bus = filas_bus,
    filas_actividad = filas_actividad,
    registros_viajes = filas_viajes,
    personas = personas,
    subidas = subidas,
    bajadas = bajadas,
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))

  referencias_relativas <- publicar_referencias_estaticas(
    directorio_regiones = directorio_regiones,
    directorio_red_canonica = directorio_red_canonica,
    directorio_publicado = directorio_publicado,
    red_canonica = as.character(manifiestos$insumos$red_canonica),
    version_regiones = as.character(manifiestos$regiones$version_regiones)
  )
  if (!file.rename(temporal, destino)) {
    stop("No se pudo promover atomicamente el directorio diario.",
      call. = FALSE)
  }
  entrada <- list(
    fecha_operativa = fecha,
    estado = "listo",
    directorio = relativo_dia,
    directorio_referencias = referencias_relativas,
    archivo_manifiesto = "manifiesto.yml",
    directorio_ocupacion = "pasadas_ocupacion",
    directorio_actividad = "actividad_paradas",
    directorio_viajes = "viajes_consulta",
    recorridos = length(recorridos),
    registros_viajes = filas_viajes,
    personas = personas,
    subidas = subidas,
    bajadas = bajadas,
    publicado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  archivo_catalogo <- registrar_dia_publicado(
    directorio_publicado, fecha, entrada
  )
  list(
    destino = destino,
    archivo_catalogo = archivo_catalogo,
    resumen = resumen,
    controles = controles
  )
}
