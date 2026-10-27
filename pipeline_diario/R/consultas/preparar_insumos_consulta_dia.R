normalizar_clave_consulta <- function(x) {
  valor <- trimws(as.character(x))
  valor[is.na(x) | !nzchar(valor) | valor == "NA"] <- NA_character_
  valor
}

tiempo_local_consulta <- function(x, zona_horaria) {
  texto <- if (inherits(x, "POSIXt")) {
    format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else {
    as.character(x)
  }
  as.numeric(as.POSIXct(
    texto, format = "%Y-%m-%d %H:%M:%S", tz = zona_horaria
  ))
}

tabla_actividad_paradas_vacia <- function() {
  data.table::data.table(
    fecha_operativa = as.Date(character()),
    recorrido = character(),
    id_version_variante = character(),
    codigo_variante = character(),
    codigo_linea = character(),
    codigo_empresa = character(),
    codigo_subsistema = integer(),
    tipo_servicio = character(),
    indice_secuencia = integer(),
    punto_canonico_id = character(),
    codigo_parada = character(),
    distancia_m = numeric(),
    tiempo = numeric(),
    metodo_tiempo = character(),
    buses = integer(),
    subidas = numeric(),
    bajadas = numeric(),
    ocupacion_personas = numeric()
  )
}

tabla_viajes_consulta_vacia <- function() {
  data.table::data.table(
    fecha_operativa = as.Date(character()),
    trip_id = integer(),
    recorrido = character(),
    id_version_variante = character(),
    codigo_variante = character(),
    codigo_linea = character(),
    codigo_empresa = character(),
    codigo_subsistema = integer(),
    tipo_servicio = character(),
    codigo_parada_origen = character(),
    codigo_parada_destino = character(),
    indice_secuencia_origen = integer(),
    indice_secuencia_destino = integer(),
    punto_canonico_origen = character(),
    punto_canonico_destino = character(),
    tiempo_observado_subida = numeric(),
    tiempo_observado_bajada = numeric(),
    tiempo_estimado_subida = numeric(),
    tiempo_estimado_bajada = numeric(),
    cantidad_pasajeros = numeric(),
    recuperada_paso10 = logical(),
    requiere_revision = logical(),
    accion_recuperacion = character()
  )
}

tabla_calidad_insumos_vacia <- function() {
  data.table::data.table(
    recorrido = character(),
    id_version_variante = character(),
    tipo_servicio = character(),
    filas_actividad = integer(),
    paradas = integer(),
    viajes = integer(),
    personas = numeric(),
    subidas = numeric(),
    bajadas = numeric(),
    pasadas_bus = numeric()
  )
}

preparar_insumos_consulta_dia <- function(
    fecha,
    archivo_od_operativa,
    directorio_resolucion_paso09,
    directorio_recuperacion_paso10,
    directorio_recorridos_paso11,
    directorio_uso_variantes_paso08,
    directorio_ocupacion_paso13,
    directorio_paradas_canonicas,
    directorio_salida,
    zona_horaria = "America/Montevideo") {
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

  padre_salida <- file.path(directorio_salida, "insumos_consulta")
  destino <- file.path(padre_salida, paste0("fecha=", fecha))
  checkpoint <- file.path(
    padre_salida, paste0(".checkpoint_fecha=", gsub("-", "", fecha))
  )
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf(
      "Los insumos de consulta ya existen y no seran sobrescritos: %s",
      destino
    ), call. = FALSE)
  }

  archivo_paso09 <- file.path(
    directorio_resolucion_paso09, "resolucion_paradas.parquet"
  )
  archivo_paso10 <- file.path(
    directorio_recuperacion_paso10, "recuperaciones_paradas.parquet"
  )
  archivo_paso11 <- file.path(
    directorio_recorridos_paso11, "recorridos_operativos.parquet"
  )
  archivo_paso08 <- file.path(
    directorio_uso_variantes_paso08, "uso_variantes.parquet"
  )
  archivo_paradas <- file.path(
    directorio_paradas_canonicas, "paradas_proyectadas.parquet"
  )
  archivo_cuarentena <- file.path(
    directorio_ocupacion_paso13,
    "viajes_cuarentena_tiempo_invertido.parquet"
  )
  archivos_manifiesto <- c(
    paso08 = file.path(directorio_uso_variantes_paso08, "manifiesto.yml"),
    paso09 = file.path(directorio_resolucion_paso09, "manifiesto.yml"),
    paso10 = file.path(directorio_recuperacion_paso10, "manifiesto.yml"),
    paso11 = file.path(directorio_recorridos_paso11, "manifiesto.yml"),
    paso13 = file.path(directorio_ocupacion_paso13, "manifiesto.yml"),
    paradas = file.path(directorio_paradas_canonicas, "manifiesto.yml")
  )
  entradas <- c(
    archivo_od_operativa, archivo_paso08, archivo_paso09, archivo_paso10,
    archivo_paso11, archivo_paradas, archivo_cuarentena,
    archivos_manifiesto
  )
  faltantes <- entradas[!file.exists(entradas)]
  directorios_partes <- file.path(
    directorio_ocupacion_paso13,
    c("pasadas_ocupacion", "calidad_ocupacion")
  )
  if (length(faltantes) || any(!dir.exists(directorios_partes))) {
    stop(sprintf(
      "Faltan entradas para preparar insumos de consulta: %s",
      paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  manifiestos <- lapply(archivos_manifiesto, yaml::read_yaml)
  valor_version <- function(x) {
    as.character(if (!is.null(x$version_registro_variantes)) {
      x$version_registro_variantes
    } else {
      x$version
    })
  }
  versiones <- vapply(manifiestos, valor_version, character(1L))
  snapshots <- vapply(
    manifiestos,
    function(x) as.character(x$snapshot_referencias),
    character(1L)
  )
  redes <- vapply(
    manifiestos, function(x) as.character(x$red_canonica), character(1L)
  )
  fechas_diarias <- vapply(
    manifiestos[c("paso08", "paso09", "paso10", "paso11", "paso13")],
    function(x) as.character(x$fecha_operativa),
    character(1L)
  )
  metadatos_coinciden <-
    all(fechas_diarias == fecha) &&
    length(unique(versiones)) == 1L &&
    length(unique(snapshots)) == 1L &&
    length(unique(redes)) == 1L &&
    !anyNA(c(versiones, snapshots, redes))
  if (!metadatos_coinciden) {
    stop("Las entradas no pertenecen a la misma entrega canonica.",
      call. = FALSE)
  }
  if (!identical(as.integer(manifiestos$paso13$controles_fallidos), 0L)) {
    stop("La ocupacion promovida contiene controles fallidos.",
      call. = FALSE)
  }

  archivos_pasadas <- sort(list.files(
    directorios_partes[[1L]], pattern = "[.]parquet$", full.names = TRUE
  ))
  archivos_calidad <- sort(list.files(
    directorios_partes[[2L]], pattern = "[.]parquet$", full.names = TRUE
  ))
  if (!length(archivos_pasadas) ||
      !identical(basename(archivos_pasadas), basename(archivos_calidad))) {
    stop("Las particiones promovidas de ocupacion estan incompletas.",
      call. = FALSE)
  }
  calidad_ocupacion <- data.table::rbindlist(lapply(
    archivos_calidad,
    function(archivo) data.table::as.data.table(arrow::read_parquet(archivo))
  ))
  if (!nrow(calidad_ocupacion) ||
      anyNA(calidad_ocupacion$recorrido) ||
      anyDuplicated(calidad_ocupacion$recorrido)) {
    stop("La calidad de ocupacion no identifica recorridos unicos.",
      call. = FALSE)
  }
  calidad_ocupacion[, `:=`(
    recorrido = as.character(recorrido),
    id_version_variante = as.character(id_version_variante),
    tipo_servicio = as.character(tipo_servicio)
  )]

  catalogo_recorridos <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso11,
    col_select = tidyselect::all_of(c(
      "recorrido", "cod_empresa", "id_version_variante", "tipo_servicio"
    ))
  ))
  catalogo_recorridos[, `:=`(
    recorrido = as.character(recorrido),
    codigo_empresa = normalizar_clave_consulta(cod_empresa),
    id_version_variante = as.character(id_version_variante),
    tipo_servicio = as.character(tipo_servicio)
  )]
  catalogo_recorridos[, cod_empresa := NULL]
  catalogo_recorridos <- catalogo_recorridos[
    recorrido %in% calidad_ocupacion$recorrido
  ]
  if (nrow(catalogo_recorridos) != nrow(calidad_ocupacion) ||
      anyDuplicated(catalogo_recorridos$recorrido)) {
    stop("El catalogo de recorridos no coincide con ocupacion.",
      call. = FALSE)
  }

  uso_variantes <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso08,
    col_select = tidyselect::all_of(c(
      "codigo_variante", "id_version_variante", "codigo_linea",
      "codigo_subsistema"
    ))
  ))
  uso_variantes[, `:=`(
    codigo_variante = normalizar_clave_consulta(codigo_variante),
    id_version_variante = as.character(id_version_variante),
    codigo_linea = normalizar_clave_consulta(codigo_linea),
    codigo_subsistema = as.integer(codigo_subsistema)
  )]
  uso_variantes <- uso_variantes[!is.na(id_version_variante)]
  uso_variantes <- unique(uso_variantes[, .(
    id_version_variante, codigo_variante, codigo_linea, codigo_subsistema
  )])
  if (anyDuplicated(uso_variantes$id_version_variante)) {
    stop("Una version de variante tiene metadatos diarios ambiguos.",
      call. = FALSE)
  }
  catalogo_recorridos <- merge(
    catalogo_recorridos, uso_variantes,
    by = "id_version_variante", all.x = TRUE, sort = FALSE
  )
  if (anyNA(catalogo_recorridos[, .(
      codigo_variante, codigo_linea, codigo_subsistema
  )]) || any(catalogo_recorridos$tipo_servicio !=
      data.table::fifelse(
        catalogo_recorridos$codigo_subsistema == 1L, "urbano", "inter"
      ))) {
    stop("Los metadatos de los recorridos no son consistentes.",
      call. = FALSE)
  }

  paradas <- data.table::as.data.table(arrow::read_parquet(
    archivo_paradas,
    col_select = tidyselect::all_of(c(
      "id_version_variante", "cod_ubic_parada", "punto_canonico_id",
      "indice_secuencia"
    ))
  ))
  paradas[, `:=`(
    id_version_variante = as.character(id_version_variante),
    codigo_parada = normalizar_clave_consulta(cod_ubic_parada),
    punto_canonico_id = as.character(punto_canonico_id),
    indice_secuencia = as.integer(indice_secuencia)
  )]
  paradas[, cod_ubic_parada := NULL]
  paradas <- unique(paradas[, .(
    id_version_variante, indice_secuencia,
    punto_canonico_id, codigo_parada
  )])
  paradas <- paradas[
    id_version_variante %in% catalogo_recorridos$id_version_variante
  ]
  if (anyNA(paradas) || anyDuplicated(paradas[, .(
      id_version_variante, indice_secuencia, codigo_parada
  )])) {
    stop("El indice de paradas canonicas es ambiguo o incompleto.",
      call. = FALSE)
  }

  od <- data.table::as.data.table(arrow::read_parquet(
    archivo_od_operativa,
    col_select = tidyselect::all_of(c(
      "trip_id", "cod_empresa", "numero_evento_real_subida",
      "fecha_evento_subida", "fecha_evento_bajada",
      "cantidad_pasajeros", "tipo_servicio"
    ))
  ))
  if (anyNA(od$trip_id) || anyDuplicated(od$trip_id)) {
    stop("La OD operativa no tiene trip_id completo y unico.",
      call. = FALSE)
  }
  od[, `:=`(
    codigo_empresa_od = normalizar_clave_consulta(cod_empresa),
    numero_evento_real_subida = normalizar_clave_consulta(
      numero_evento_real_subida
    ),
    cantidad_pasajeros = as.numeric(cantidad_pasajeros),
    tipo_servicio_od = as.character(tipo_servicio),
    tiempo_observado_subida = tiempo_local_consulta(
      fecha_evento_subida, zona_horaria
    ),
    tiempo_observado_bajada = tiempo_local_consulta(
      fecha_evento_bajada, zona_horaria
    )
  )]
  if (any(!is.finite(od$cantidad_pasajeros)) ||
      any(od$cantidad_pasajeros < 0) ||
      any(od$cantidad_pasajeros != floor(od$cantidad_pasajeros))) {
    stop("La OD contiene cantidades de pasajeros invalidas.",
      call. = FALSE)
  }
  od[, recorrido := data.table::fifelse(
    !is.na(codigo_empresa_od) & !is.na(numero_evento_real_subida),
    paste(fecha, codigo_empresa_od, numero_evento_real_subida, sep = "|"),
    NA_character_
  )]
  od[, c(
    "cod_empresa", "numero_evento_real_subida", "fecha_evento_subida",
    "fecha_evento_bajada", "tipo_servicio"
  ) := NULL]

  paso09 <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso09,
    col_select = tidyselect::all_of(c(
      "trip_id", "codigo_variante_subida", "codigo_parada_subida",
      "codigo_parada_bajada", "id_version_variante",
      "indice_secuencia_subida", "indice_secuencia_bajada",
      "punto_canonico_subida", "punto_canonico_bajada",
      "estado_resolucion"
    ))
  ))
  paso10 <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso10,
    col_select = tidyselect::all_of(c(
      "trip_id", "codigo_variante_subida",
      "id_version_variante_subida", "codigo_parada_subida_original",
      "indice_secuencia_subida", "punto_canonico_subida",
      "codigo_parada_bajada_asignada",
      "indice_secuencia_bajada_asignado",
      "punto_canonico_bajada_asignado", "recuperada",
      "requiere_revision", "accion_resolucion"
    ))
  ))
  if (anyNA(paso09$trip_id) || anyDuplicated(paso09$trip_id) ||
      !setequal(paso09$trip_id, od$trip_id) ||
      anyNA(paso10$trip_id) || anyDuplicated(paso10$trip_id) ||
      !all(paso10$trip_id %in% paso09$trip_id)) {
    stop("Los pasos 09 o 10 tienen claves trip_id invalidas.",
      call. = FALSE)
  }
  paso09[, `:=`(
    codigo_variante_subida = normalizar_clave_consulta(
      codigo_variante_subida
    ),
    codigo_parada_subida = normalizar_clave_consulta(codigo_parada_subida),
    codigo_parada_bajada = normalizar_clave_consulta(codigo_parada_bajada),
    id_version_variante = as.character(id_version_variante),
    indice_secuencia_subida = as.integer(indice_secuencia_subida),
    indice_secuencia_bajada = as.integer(indice_secuencia_bajada),
    punto_canonico_subida = as.character(punto_canonico_subida),
    punto_canonico_bajada = as.character(punto_canonico_bajada),
    estado_resolucion = as.character(estado_resolucion)
  )]
  delta <- paso10[, .(
    trip_id,
    codigo_variante_recuperada = normalizar_clave_consulta(
      codigo_variante_subida
    ),
    id_version_variante_recuperada = as.character(
      id_version_variante_subida
    ),
    codigo_parada_origen_recuperada = normalizar_clave_consulta(
      codigo_parada_subida_original
    ),
    indice_origen_recuperado = as.integer(indice_secuencia_subida),
    punto_origen_recuperado = as.character(punto_canonico_subida),
    codigo_parada_destino_recuperada = normalizar_clave_consulta(
      codigo_parada_bajada_asignada
    ),
    indice_destino_recuperado = as.integer(
      indice_secuencia_bajada_asignado
    ),
    punto_destino_recuperado = as.character(
      punto_canonico_bajada_asignado
    ),
    recuperada = as.logical(recuperada),
    requiere_revision = as.logical(requiere_revision),
    accion_recuperacion = as.character(accion_resolucion)
  )]
  base <- merge(paso09, od, by = "trip_id", all.x = TRUE, sort = FALSE)
  base <- merge(base, delta, by = "trip_id", all.x = TRUE, sort = FALSE)
  if (nrow(base) != nrow(od) || anyDuplicated(base$trip_id)) {
    stop("La reconstruccion altero el universo OD.", call. = FALSE)
  }
  base[, `:=`(
    apta_base = estado_resolucion == "apta_secuencia",
    recuperada_paso10 =
      estado_resolucion == "variante_subida_bajada_distinta" &
      !is.na(recuperada) & recuperada == TRUE
  )]
  base[, viaje_elegible := apta_base | recuperada_paso10]
  base[, `:=`(
    codigo_variante = data.table::fifelse(
      recuperada_paso10,
      codigo_variante_recuperada,
      data.table::fifelse(apta_base, codigo_variante_subida, NA_character_)
    ),
    id_version_variante_efectiva = data.table::fifelse(
      recuperada_paso10,
      id_version_variante_recuperada,
      data.table::fifelse(apta_base, id_version_variante, NA_character_)
    ),
    codigo_parada_origen = data.table::fifelse(
      recuperada_paso10,
      codigo_parada_origen_recuperada,
      data.table::fifelse(apta_base, codigo_parada_subida, NA_character_)
    ),
    codigo_parada_destino = data.table::fifelse(
      recuperada_paso10,
      codigo_parada_destino_recuperada,
      data.table::fifelse(apta_base, codigo_parada_bajada, NA_character_)
    ),
    indice_secuencia_origen = data.table::fifelse(
      recuperada_paso10,
      indice_origen_recuperado,
      data.table::fifelse(
        apta_base, indice_secuencia_subida, NA_integer_
      )
    ),
    indice_secuencia_destino = data.table::fifelse(
      recuperada_paso10,
      indice_destino_recuperado,
      data.table::fifelse(
        apta_base, indice_secuencia_bajada, NA_integer_
      )
    ),
    punto_canonico_origen = data.table::fifelse(
      recuperada_paso10,
      punto_origen_recuperado,
      data.table::fifelse(apta_base, punto_canonico_subida, NA_character_)
    ),
    punto_canonico_destino = data.table::fifelse(
      recuperada_paso10,
      punto_destino_recuperado,
      data.table::fifelse(apta_base, punto_canonico_bajada, NA_character_)
    )
  )]
  viajes_pre_cuarentena <- base[
    viaje_elegible == TRUE & recorrido %in% calidad_ocupacion$recorrido
  ]
  mapa_recorridos <- catalogo_recorridos[, .(
    recorrido,
    id_version_variante_catalogo = id_version_variante,
    codigo_variante_catalogo = codigo_variante,
    codigo_linea,
    codigo_empresa,
    codigo_subsistema,
    tipo_servicio = tipo_servicio
  )]
  viajes_pre_cuarentena <- merge(
    viajes_pre_cuarentena, mapa_recorridos,
    by = "recorrido", all.x = TRUE, sort = FALSE
  )
  if (viajes_pre_cuarentena[, any(
      is.na(id_version_variante_efectiva) |
      is.na(codigo_variante) |
      is.na(codigo_parada_origen) |
      is.na(codigo_parada_destino) |
      is.na(indice_secuencia_origen) |
      is.na(indice_secuencia_destino) |
      is.na(punto_canonico_origen) |
      is.na(punto_canonico_destino) |
      indice_secuencia_destino <= indice_secuencia_origen
  )] || any(viajes_pre_cuarentena$id_version_variante_efectiva !=
      viajes_pre_cuarentena$id_version_variante_catalogo) ||
      any(viajes_pre_cuarentena$codigo_variante !=
        viajes_pre_cuarentena$codigo_variante_catalogo) ||
      any(viajes_pre_cuarentena$codigo_empresa_od !=
        viajes_pre_cuarentena$codigo_empresa) ||
      any(viajes_pre_cuarentena$tipo_servicio_od !=
        viajes_pre_cuarentena$tipo_servicio)) {
    stop("Los viajes reconstruidos no coinciden con sus recorridos.",
      call. = FALSE)
  }

  cuarentena <- data.table::as.data.table(arrow::read_parquet(
    archivo_cuarentena,
    col_select = tidyselect::all_of(c("trip_id", "cantidad_pasajeros"))
  ))
  invertidos_calculados <- viajes_pre_cuarentena[
    is.finite(tiempo_observado_subida) &
      is.finite(tiempo_observado_bajada) &
      tiempo_observado_bajada < tiempo_observado_subida
  ]
  if (anyNA(cuarentena$trip_id) || anyDuplicated(cuarentena$trip_id) ||
      !setequal(cuarentena$trip_id, invertidos_calculados$trip_id) ||
      sum(as.numeric(cuarentena$cantidad_pasajeros)) !=
        sum(invertidos_calculados$cantidad_pasajeros)) {
    stop("La cuarentena temporal no coincide con la ocupacion promovida.",
      call. = FALSE)
  }
  viajes <- viajes_pre_cuarentena[!trip_id %in% cuarentena$trip_id]
  if (nrow(viajes) != sum(calidad_ocupacion$registros_ocupacion) ||
      sum(viajes$cantidad_pasajeros) !=
        sum(calidad_ocupacion$personas_ocupacion)) {
    stop("Los viajes de consulta no coinciden con la calidad de ocupacion.",
      call. = FALSE)
  }

  configuracion_checkpoint <- list(
    fecha_operativa = fecha,
    version_registro_variantes = versiones[[1L]],
    snapshot_referencias = snapshots[[1L]],
    red_canonica = redes[[1L]],
    zona_horaria = zona_horaria,
    fuente_ocupacion = normalizePath(
      directorio_ocupacion_paso13, winslash = "/", mustWork = TRUE
    ),
    cantidad_lotes = length(archivos_pasadas),
    recorridos = nrow(calidad_ocupacion)
  )
  archivo_checkpoint <- file.path(checkpoint, "checkpoint.yml")
  if (dir.exists(checkpoint)) {
    if (!file.exists(archivo_checkpoint)) {
      stop("El checkpoint de insumos esta incompleto.", call. = FALSE)
    }
    existente <- yaml::read_yaml(archivo_checkpoint)
    campos <- names(configuracion_checkpoint)
    if (!identical(
        as.character(unlist(existente[campos])),
        as.character(unlist(configuracion_checkpoint[campos]))
    )) {
      stop("El checkpoint de insumos pertenece a otra configuracion.",
        call. = FALSE)
    }
    subdirectorios <- file.path(
      checkpoint,
      c("actividad_paradas", "viajes_consulta", "calidad_insumos")
    )
    if (any(!dir.exists(subdirectorios))) {
      stop("El checkpoint no conserva sus particiones.", call. = FALSE)
    }
  } else {
    dir.create(file.path(checkpoint, "actividad_paradas"), recursive = TRUE)
    dir.create(file.path(checkpoint, "viajes_consulta"), recursive = TRUE)
    dir.create(file.path(checkpoint, "calidad_insumos"), recursive = TRUE)
    yaml::write_yaml(configuracion_checkpoint, archivo_checkpoint)
  }

  for (indice_lote in seq_along(archivos_pasadas)) {
    nombre_lote <- basename(archivos_pasadas[[indice_lote]])
    archivo_actividad <- file.path(
      checkpoint, "actividad_paradas", nombre_lote
    )
    archivo_viajes <- file.path(checkpoint, "viajes_consulta", nombre_lote)
    archivo_calidad_salida <- file.path(
      checkpoint, "calidad_insumos", nombre_lote
    )
    if (all(file.exists(c(
        archivo_actividad, archivo_viajes, archivo_calidad_salida
    )))) {
      message(sprintf(
        "[insumos] Lote %s/%s reutilizado.",
        indice_lote, length(archivos_pasadas)
      ))
      next
    }

    pasadas <- data.table::as.data.table(arrow::read_parquet(
      archivos_pasadas[[indice_lote]]
    ))
    pasadas[, `:=`(
      recorrido = as.character(recorrido),
      id_version_variante = as.character(id_version_variante),
      tipo_servicio = as.character(tipo_servicio),
      indice_secuencia = as.integer(indice_secuencia),
      punto_canonico_id = as.character(punto_canonico_id),
      subidas_posicion = as.numeric(subidas),
      bajadas_posicion = as.numeric(bajadas)
    )]
    pasadas[, c("subidas", "bajadas") := NULL]
    recorridos_lote <- unique(pasadas$recorrido)
    viajes_lote <- viajes[recorrido %in% recorridos_lote]

    mapa_tiempos_origen <- pasadas[, .(
      recorrido,
      indice_secuencia_origen = indice_secuencia,
      punto_canonico_origen = punto_canonico_id,
      tiempo_estimado_subida = as.numeric(tiempo)
    )]
    mapa_tiempos_destino <- pasadas[, .(
      recorrido,
      indice_secuencia_destino = indice_secuencia,
      punto_canonico_destino = punto_canonico_id,
      tiempo_estimado_bajada = as.numeric(tiempo)
    )]
    viajes_lote <- merge(
      viajes_lote, mapa_tiempos_origen,
      by = c(
        "recorrido", "indice_secuencia_origen", "punto_canonico_origen"
      ),
      all.x = TRUE, sort = FALSE
    )
    viajes_lote <- merge(
      viajes_lote, mapa_tiempos_destino,
      by = c(
        "recorrido", "indice_secuencia_destino", "punto_canonico_destino"
      ),
      all.x = TRUE, sort = FALSE
    )
    if (anyNA(viajes_lote[, .(
        tiempo_estimado_subida, tiempo_estimado_bajada
    )])) {
      stop("Hay viajes sin tiempos estimados en sus extremos.",
        call. = FALSE)
    }

    indice_lote_paradas <- paradas[
      id_version_variante %in% pasadas$id_version_variante
    ]
    actividad <- merge(
      pasadas, indice_lote_paradas,
      by = c(
        "id_version_variante", "indice_secuencia", "punto_canonico_id"
      ),
      all = FALSE, allow.cartesian = TRUE, sort = FALSE
    )
    metadatos_lote <- catalogo_recorridos[
      recorrido %in% recorridos_lote,
      .(
        recorrido, codigo_variante, codigo_linea, codigo_empresa,
        codigo_subsistema
      )
    ]
    actividad <- merge(
      actividad, metadatos_lote,
      by = "recorrido", all.x = TRUE, sort = FALSE
    )
    if (!nrow(actividad) || anyNA(actividad[, .(
        recorrido, codigo_variante, codigo_linea, codigo_empresa,
        codigo_subsistema, codigo_parada
    )])) {
      stop("La actividad de paradas perdio claves obligatorias.",
        call. = FALSE)
    }
    claves_actividad <- actividad[, .(
      recorrido, indice_secuencia, punto_canonico_id, codigo_parada
    )]
    if (anyDuplicated(claves_actividad)) {
      stop("La actividad duplica una parada dentro del recorrido.",
        call. = FALSE)
    }

    subidas <- viajes_lote[, .(
      subidas = sum(cantidad_pasajeros)
    ), by = .(
      recorrido,
      indice_secuencia = indice_secuencia_origen,
      punto_canonico_id = punto_canonico_origen,
      codigo_parada = codigo_parada_origen
    )]
    bajadas <- viajes_lote[, .(
      bajadas = sum(cantidad_pasajeros)
    ), by = .(
      recorrido,
      indice_secuencia = indice_secuencia_destino,
      punto_canonico_id = punto_canonico_destino,
      codigo_parada = codigo_parada_destino
    )]
    if (nrow(subidas[!claves_actividad, on = names(claves_actividad)]) ||
        nrow(bajadas[!claves_actividad, on = names(claves_actividad)])) {
      stop("Hay extremos de viaje fuera de la actividad de paradas.",
        call. = FALSE)
    }
    actividad[, `:=`(subidas = 0, bajadas = 0)]
    actividad[subidas, on = names(claves_actividad), subidas := i.subidas]
    actividad[bajadas, on = names(claves_actividad), bajadas := i.bajadas]
    claves_movimiento_unicas <- actividad[, .(
      subidas_unicas = data.table::uniqueN(subidas_posicion),
      bajadas_unicas = data.table::uniqueN(bajadas_posicion)
    ), by = .(recorrido, indice_secuencia)]
    if (any(claves_movimiento_unicas$subidas_unicas != 1L) ||
        any(claves_movimiento_unicas$bajadas_unicas != 1L)) {
      stop("Una posicion tiene movimientos de ocupacion ambiguos.",
        call. = FALSE)
    }
    balance_posicion <- actividad[, .(
      subidas = sum(subidas),
      bajadas = sum(bajadas),
      subidas_posicion = subidas_posicion[[1L]],
      bajadas_posicion = bajadas_posicion[[1L]]
    ), by = .(recorrido, indice_secuencia)]
    if (any(balance_posicion$subidas !=
        balance_posicion$subidas_posicion) ||
        any(balance_posicion$bajadas !=
          balance_posicion$bajadas_posicion)) {
      stop("La actividad no conserva los movimientos de ocupacion.",
        call. = FALSE)
    }
    actividad[, c("subidas_posicion", "bajadas_posicion") := NULL]
    actividad[, fecha_operativa := fecha_date]
    data.table::setcolorder(
      actividad, names(tabla_actividad_paradas_vacia())
    )

    salida_viajes <- viajes_lote[, .(
      fecha_operativa = fecha_date,
      trip_id,
      recorrido,
      id_version_variante = id_version_variante_efectiva,
      codigo_variante,
      codigo_linea,
      codigo_empresa,
      codigo_subsistema,
      tipo_servicio,
      codigo_parada_origen,
      codigo_parada_destino,
      indice_secuencia_origen,
      indice_secuencia_destino,
      punto_canonico_origen,
      punto_canonico_destino,
      tiempo_observado_subida,
      tiempo_observado_bajada,
      tiempo_estimado_subida,
      tiempo_estimado_bajada,
      cantidad_pasajeros,
      recuperada_paso10,
      requiere_revision = data.table::fcoalesce(requiere_revision, FALSE),
      accion_recuperacion
    )]
    data.table::setcolorder(
      salida_viajes, names(tabla_viajes_consulta_vacia())
    )

    identidad <- unique(pasadas[, .(
      recorrido, id_version_variante, tipo_servicio
    )])
    resumen_actividad <- actividad[, .(
      filas_actividad = .N,
      paradas = data.table::uniqueN(codigo_parada),
      subidas = sum(subidas),
      bajadas = sum(bajadas),
      pasadas_bus = sum(as.numeric(buses))
    ), by = recorrido]
    resumen_viajes <- salida_viajes[, .(
      viajes = .N,
      personas = sum(cantidad_pasajeros)
    ), by = recorrido]
    calidad_lote <- merge(
      identidad, resumen_actividad,
      by = "recorrido", all.x = TRUE, sort = FALSE
    )
    calidad_lote <- merge(
      calidad_lote, resumen_viajes,
      by = "recorrido", all.x = TRUE, sort = FALSE
    )
    columnas_cero <- c(
      "filas_actividad", "paradas", "viajes", "personas",
      "subidas", "bajadas", "pasadas_bus"
    )
    for (columna in columnas_cero) {
      data.table::set(
        calidad_lote, which(is.na(calidad_lote[[columna]])), columna, 0
      )
    }
    data.table::setcolorder(
      calidad_lote, names(tabla_calidad_insumos_vacia())
    )

    salidas_lote <- list(
      actividad = list(actividad, archivo_actividad),
      viajes = list(salida_viajes, archivo_viajes),
      calidad = list(calidad_lote, archivo_calidad_salida)
    )
    for (salida in salidas_lote) {
      if (file.exists(salida[[2L]])) next
      archivo_temporal <- paste0(salida[[2L]], ".tmp")
      arrow::write_parquet(
        salida[[1L]], archivo_temporal, compression = "zstd"
      )
      if (!file.rename(archivo_temporal, salida[[2L]])) {
        stop("No se pudo promover una particion de insumos.",
          call. = FALSE)
      }
    }
    message(sprintf(
      "[insumos] Lote %s/%s terminado.",
      indice_lote, length(archivos_pasadas)
    ))
  }

  directorios_salida <- file.path(
    checkpoint, c("actividad_paradas", "viajes_consulta", "calidad_insumos")
  )
  partes_salida <- lapply(directorios_salida, function(x) sort(list.files(
    x, pattern = "[.]parquet$", full.names = TRUE
  )))
  nombres_esperados <- basename(archivos_pasadas)
  partes_completas <- all(vapply(
    partes_salida,
    function(x) identical(basename(x), nombres_esperados),
    logical(1L)
  ))
  if (!partes_completas) {
    stop("No se completaron todas las particiones de insumos.",
      call. = FALSE)
  }

  calidad <- data.table::rbindlist(lapply(
    partes_salida[[3L]],
    function(x) data.table::as.data.table(arrow::read_parquet(x))
  ))
  if (nrow(calidad) != nrow(calidad_ocupacion) ||
      anyDuplicated(calidad$recorrido) ||
      !setequal(calidad$recorrido, calidad_ocupacion$recorrido)) {
    stop("La calidad de insumos no conserva los recorridos.",
      call. = FALSE)
  }

  resumen_actividad_base <- calidad[, .(
    filas = sum(filas_actividad),
    recorridos = .N,
    subidas = sum(subidas),
    bajadas = sum(bajadas),
    pasadas_bus = sum(pasadas_bus)
  ), by = tipo_servicio]
  resumen_actividad_total <- calidad[, .(
    tipo_servicio = "total",
    filas = sum(filas_actividad),
    recorridos = .N,
    subidas = sum(subidas),
    bajadas = sum(bajadas),
    pasadas_bus = sum(pasadas_bus)
  )]
  resumen_actividad <- data.table::rbindlist(list(
    resumen_actividad_base, resumen_actividad_total
  ))[order(tipo_servicio)]
  resumen_viajes_base <- calidad[, .(
    registros = sum(viajes),
    personas = sum(personas),
    recorridos = sum(viajes > 0)
  ), by = tipo_servicio]
  resumen_viajes_total <- calidad[, .(
    tipo_servicio = "total",
    registros = sum(viajes),
    personas = sum(personas),
    recorridos = sum(viajes > 0)
  )]
  resumen_viajes <- data.table::rbindlist(list(
    resumen_viajes_base, resumen_viajes_total
  ))[order(tipo_servicio)]

  filas_actividad <- 0
  filas_viajes <- 0
  viajes_ids <- vector("list", length(partes_salida[[2L]]))
  paradas_activas <- vector("list", length(partes_salida[[1L]]))
  actividad_duplicada <- FALSE
  actividad_incompleta <- FALSE
  viajes_incompletos <- FALSE
  for (i in seq_along(partes_salida[[1L]])) {
    actividad <- data.table::as.data.table(arrow::read_parquet(
      partes_salida[[1L]][[i]],
      col_select = tidyselect::all_of(c(
        "recorrido", "indice_secuencia", "codigo_parada", "subidas",
        "bajadas", "buses", "tipo_servicio"
      ))
    ))
    consulta <- data.table::as.data.table(arrow::read_parquet(
      partes_salida[[2L]][[i]],
      col_select = tidyselect::all_of(c(
        "trip_id", "recorrido", "indice_secuencia_origen",
        "indice_secuencia_destino", "tiempo_estimado_subida",
        "tiempo_estimado_bajada", "cantidad_pasajeros", "tipo_servicio"
      ))
    ))
    filas_actividad <- filas_actividad + nrow(actividad)
    filas_viajes <- filas_viajes + nrow(consulta)
    viajes_ids[[i]] <- consulta$trip_id
    paradas_activas[[i]] <- unique(actividad[, .(
      tipo_servicio, codigo_parada
    )])
    actividad_duplicada <- actividad_duplicada || anyDuplicated(
      actividad[, .(recorrido, indice_secuencia, codigo_parada)]
    ) > 0L
    actividad_incompleta <- actividad_incompleta ||
      anyNA(actividad[, .(
        recorrido, indice_secuencia, codigo_parada,
        subidas, bajadas, buses, tipo_servicio
      )])
    viajes_incompletos <- viajes_incompletos ||
      anyNA(consulta[, .(
        trip_id, recorrido, indice_secuencia_origen,
        indice_secuencia_destino, tiempo_estimado_subida,
        tiempo_estimado_bajada, cantidad_pasajeros, tipo_servicio
      )])
  }
  ids_salida <- unlist(viajes_ids, use.names = FALSE)
  paradas_activas <- unique(data.table::rbindlist(paradas_activas))
  cantidad_paradas <- paradas_activas[, .(
    paradas = data.table::uniqueN(codigo_parada)
  ), by = tipo_servicio]
  resumen_actividad <- merge(
    resumen_actividad, cantidad_paradas,
    by = "tipo_servicio", all.x = TRUE, sort = FALSE
  )
  resumen_actividad[
    tipo_servicio == "total",
    paradas := data.table::uniqueN(paradas_activas$codigo_parada)
  ]
  data.table::setcolorder(resumen_actividad, c(
    "tipo_servicio", "filas", "recorridos", "paradas",
    "subidas", "bajadas", "pasadas_bus"
  ))
  data.table::setorder(resumen_actividad, tipo_servicio)
  personas_esperadas <- sum(viajes$cantidad_pasajeros)
  controles <- data.table::data.table(
    control = c(
      "entregas_canonicas_coinciden",
      "ocupacion_aprobada",
      "particiones_entrada_completas",
      "catalogo_recorridos_coincide",
      "metadatos_variantes_coinciden",
      "viajes_reconstruidos_coinciden",
      "cuarentena_tiempo_respetada",
      "extremos_presentes_en_paradas",
      "particiones_salida_completas",
      "calidad_conserva_recorridos",
      "actividad_sin_duplicados",
      "actividad_completa",
      "viajes_unicos",
      "viajes_completos",
      "viajes_con_orden_positivo",
      "subidas_conservan_personas",
      "bajadas_conservan_personas",
      "actividad_coincide_con_ocupacion",
      "viajes_coinciden_con_ocupacion"
    ),
    aprobado = c(
      metadatos_coinciden,
      identical(as.integer(manifiestos$paso13$controles_fallidos), 0L),
      identical(basename(archivos_pasadas), basename(archivos_calidad)),
      nrow(catalogo_recorridos) == nrow(calidad_ocupacion),
      !anyNA(catalogo_recorridos[, .(
        codigo_variante, codigo_linea, codigo_subsistema
      )]),
      nrow(viajes) == sum(calidad_ocupacion$registros_ocupacion) &&
        sum(viajes$cantidad_pasajeros) ==
          sum(calidad_ocupacion$personas_ocupacion),
      !any(cuarentena$trip_id %in% ids_salida),
      TRUE,
      partes_completas,
      nrow(calidad) == nrow(calidad_ocupacion) &&
        setequal(calidad$recorrido, calidad_ocupacion$recorrido),
      !actividad_duplicada,
      !actividad_incompleta,
      length(ids_salida) == length(unique(ids_salida)),
      !viajes_incompletos,
      all(viajes$indice_secuencia_destino >
        viajes$indice_secuencia_origen),
      sum(calidad$subidas) == personas_esperadas,
      sum(calidad$bajadas) == personas_esperadas,
      filas_actividad == sum(calidad$filas_actividad) &&
        sum(calidad$subidas) == sum(calidad_ocupacion$subidas) &&
        sum(calidad$bajadas) == sum(calidad_ocupacion$bajadas),
      filas_viajes == nrow(viajes) &&
        setequal(ids_salida, viajes$trip_id)
    )
  )
  controles[, estado := data.table::fifelse(
    aprobado, "aprobado", "fallo"
  )]
  if (any(!controles$aprobado)) {
    stop("Los insumos de consulta no aprobaron sus controles.",
      call. = FALSE)
  }

  data.table::fwrite(
    resumen_actividad,
    file.path(checkpoint, "resumen_actividad_paradas.csv")
  )
  data.table::fwrite(
    resumen_viajes,
    file.path(checkpoint, "resumen_viajes_consulta.csv")
  )
  data.table::fwrite(controles, file.path(checkpoint, "controles.csv"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    tipo_salida = "insumos_consulta_particionados",
    fuente_ocupacion = normalizePath(
      directorio_ocupacion_paso13, winslash = "/", mustWork = TRUE
    ),
    version_registro_variantes = versiones[[1L]],
    snapshot_referencias = snapshots[[1L]],
    red_canonica = redes[[1L]],
    zona_horaria = zona_horaria,
    cantidad_lotes = length(archivos_pasadas),
    recorridos = nrow(calidad),
    filas_actividad = filas_actividad,
    registros_viajes = filas_viajes,
    personas = personas_esperadas,
    subidas = sum(calidad$subidas),
    bajadas = sum(calidad$bajadas),
    pasadas_bus = sum(calidad$pasadas_bus),
    incluye_tiempos_observados = TRUE,
    incluye_tiempos_estimados = TRUE,
    incluye_regiones = FALSE,
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(checkpoint, "manifiesto.yml"))
  if (!file.rename(checkpoint, destino)) {
    stop(sprintf("No se pudo promover la salida de insumos: %s", destino),
      call. = FALSE)
  }
  list(
    destino = destino,
    calidad = calidad,
    resumen_actividad = resumen_actividad,
    resumen_viajes = resumen_viajes,
    controles = controles
  )
}
