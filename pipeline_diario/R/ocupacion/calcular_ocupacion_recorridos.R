normalizar_clave_ocupacion <- function(x) {
  valor <- trimws(as.character(x))
  valor[is.na(x) | !nzchar(valor) | valor == "NA"] <- NA_character_
  valor
}

tiempo_local_ocupacion <- function(x, zona_horaria) {
  texto <- if (inherits(x, "POSIXt")) {
    format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else {
    as.character(x)
  }
  as.numeric(as.POSIXct(
    texto, format = "%Y-%m-%d %H:%M:%S", tz = zona_horaria
  ))
}

tabla_pasadas_ocupacion_vacia <- function() {
  data.table::data.table(
    recorrido = character(),
    id_version_variante = character(),
    tipo_servicio = character(),
    indice_secuencia = integer(),
    punto_canonico_id = character(),
    distancia_m = numeric(),
    tiempo = numeric(),
    metodo_tiempo = character(),
    buses = integer(),
    subidas = numeric(),
    bajadas = numeric(),
    ocupacion_personas = numeric()
  )
}

tabla_calidad_ocupacion_vacia <- function() {
  data.table::data.table(
    recorrido = character(),
    id_version_variante = character(),
    tipo_servicio = character(),
    registros_antes_cuarentena = integer(),
    personas_antes_cuarentena = numeric(),
    viajes_cuarentena_tiempo_invertido = integer(),
    personas_cuarentena_tiempo_invertido = numeric(),
    registros_ocupacion = integer(),
    personas_ocupacion = numeric(),
    posiciones = integer(),
    subidas = numeric(),
    bajadas = numeric(),
    personas_tramo = numeric(),
    ocupacion_minima = numeric(),
    ocupacion_maxima = numeric(),
    saldo_final = numeric()
  )
}

calcular_ocupacion_lote <- function(pasadas, viajes) {
  posiciones <- data.table::copy(data.table::as.data.table(pasadas))
  tramos <- data.table::copy(data.table::as.data.table(viajes))
  columnas_pasadas <- c(
    "recorrido", "id_version_variante", "tipo_servicio",
    "indice_secuencia", "punto_canonico_id", "distancia_m", "tiempo",
    "metodo_tiempo", "buses"
  )
  columnas_viajes <- c(
    "trip_id", "recorrido", "indice_subida_efectivo",
    "indice_bajada_efectivo", "cantidad_pasajeros"
  )
  faltantes_pasadas <- setdiff(columnas_pasadas, names(posiciones))
  faltantes_viajes <- setdiff(columnas_viajes, names(tramos))
  if (length(faltantes_pasadas) || length(faltantes_viajes)) {
    stop("Faltan columnas para calcular ocupacion.", call. = FALSE)
  }
  if (!nrow(posiciones)) {
    if (nrow(tramos)) {
      stop("Hay viajes sin pasadas aprobadas.", call. = FALSE)
    }
    return(tabla_pasadas_ocupacion_vacia())
  }
  posiciones[, `:=`(
    recorrido = as.character(recorrido),
    id_version_variante = as.character(id_version_variante),
    tipo_servicio = as.character(tipo_servicio),
    indice_secuencia = as.integer(indice_secuencia),
    punto_canonico_id = as.character(punto_canonico_id),
    distancia_m = as.numeric(distancia_m),
    tiempo = as.numeric(tiempo),
    metodo_tiempo = as.character(metodo_tiempo),
    buses = as.integer(buses)
  )]
  tramos[, `:=`(
    recorrido = as.character(recorrido),
    indice_subida_efectivo = as.integer(indice_subida_efectivo),
    indice_bajada_efectivo = as.integer(indice_bajada_efectivo),
    cantidad_pasajeros = as.numeric(cantidad_pasajeros)
  )]
  if (anyNA(posiciones[, .(
      recorrido, id_version_variante, tipo_servicio, indice_secuencia,
      punto_canonico_id, metodo_tiempo, buses
  )]) || any(!is.finite(posiciones$distancia_m)) ||
      any(!is.finite(posiciones$tiempo)) ||
      anyDuplicated(posiciones[, .(recorrido, indice_secuencia)]) ||
      any(posiciones$buses != 1L)) {
    stop("Las pasadas aprobadas tienen claves o valores invalidos.",
      call. = FALSE)
  }
  if (anyNA(tramos[, .(
      trip_id, recorrido, indice_subida_efectivo,
      indice_bajada_efectivo, cantidad_pasajeros
  )]) || anyDuplicated(tramos$trip_id) ||
      any(!is.finite(tramos$cantidad_pasajeros)) ||
      any(tramos$cantidad_pasajeros < 0) ||
      any(tramos$cantidad_pasajeros != floor(tramos$cantidad_pasajeros)) ||
      any(tramos$indice_bajada_efectivo <= tramos$indice_subida_efectivo)) {
    stop("Los viajes de ocupacion tienen claves o valores invalidos.",
      call. = FALSE)
  }
  if (nrow(tramos) && !all(tramos$recorrido %in% posiciones$recorrido)) {
    stop("Hay viajes cuyo recorrido no tiene pasadas aprobadas.",
      call. = FALSE)
  }
  claves_posicion <- posiciones[, .(recorrido, indice_secuencia)]
  extremos <- data.table::rbindlist(list(
    tramos[, .(
      recorrido,
      indice_secuencia = indice_subida_efectivo,
      extremo = "subida"
    )],
    tramos[, .(
      recorrido,
      indice_secuencia = indice_bajada_efectivo,
      extremo = "bajada"
    )]
  ))
  if (nrow(extremos) && nrow(extremos[!claves_posicion,
      on = .(recorrido, indice_secuencia)])) {
    stop("Hay extremos de viaje fuera de las pasadas aprobadas.",
      call. = FALSE)
  }

  subidas <- tramos[, .(
    subidas = sum(cantidad_pasajeros)
  ), by = .(recorrido, indice_secuencia = indice_subida_efectivo)]
  bajadas <- tramos[, .(
    bajadas = sum(cantidad_pasajeros)
  ), by = .(recorrido, indice_secuencia = indice_bajada_efectivo)]
  posiciones[, `:=`(subidas = 0, bajadas = 0)]
  posiciones[subidas, on = .(recorrido, indice_secuencia),
    subidas := i.subidas]
  posiciones[bajadas, on = .(recorrido, indice_secuencia),
    bajadas := i.bajadas]
  data.table::setorder(posiciones, recorrido, indice_secuencia)
  posiciones[, ocupacion_personas := cumsum(subidas - bajadas),
    by = recorrido]
  posiciones
}

calcular_ocupacion_recorridos <- function(
    fecha,
    archivo_od_operativa,
    directorio_resolucion_paso09,
    directorio_recuperacion_paso10,
    directorio_recorridos_paso11,
    directorio_horarios_paso12,
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

  padre_salida <- file.path(directorio_salida, "ocupacion_recorridos")
  destino <- file.path(padre_salida, paste0("fecha=", fecha))
  checkpoint <- file.path(
    padre_salida, paste0(".checkpoint_fecha=", gsub("-", "", fecha))
  )
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf(
      "La ocupacion ya existe y no sera sobrescrita: %s", destino
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
  manifiestos_archivos <- c(
    paso09 = file.path(directorio_resolucion_paso09, "manifiesto.yml"),
    paso10 = file.path(directorio_recuperacion_paso10, "manifiesto.yml"),
    paso11 = file.path(directorio_recorridos_paso11, "manifiesto.yml"),
    paso12 = file.path(directorio_horarios_paso12, "manifiesto.yml")
  )
  directorio_pasadas <- file.path(directorio_horarios_paso12, "pasadas_bus")
  directorio_calidad <- file.path(
    directorio_horarios_paso12, "calidad_recorridos"
  )
  entradas <- c(
    archivo_od_operativa, archivo_paso09, archivo_paso10, archivo_paso11,
    manifiestos_archivos
  )
  faltantes <- entradas[!file.exists(entradas)]
  if (length(faltantes) || !dir.exists(directorio_pasadas) ||
      !dir.exists(directorio_calidad)) {
    stop(sprintf(
      "Faltan entradas para ocupacion: %s",
      paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  manifiestos <- lapply(manifiestos_archivos, yaml::read_yaml)
  fechas_manifiesto <- vapply(
    manifiestos, function(x) as.character(x$fecha_operativa), character(1L)
  )
  metadatos <- vapply(manifiestos, function(x) paste(
    as.character(x$version_registro_variantes),
    as.character(x$snapshot_referencias),
    as.character(x$red_canonica),
    sep = "|"
  ), character(1L))
  metadatos_coinciden <-
    all(fechas_manifiesto == fecha) && !anyNA(metadatos) &&
    length(unique(metadatos)) == 1L
  if (!metadatos_coinciden) {
    stop("Los pasos 09 a 12 no usan la misma fecha y entrega canonica.",
      call. = FALSE)
  }
  metadatos_salida <- c(
    version = as.character(
      manifiestos$paso12$version_registro_variantes
    ),
    snapshot = as.character(manifiestos$paso12$snapshot_referencias),
    red = as.character(manifiestos$paso12$red_canonica)
  )

  archivos_pasadas_entrada <- sort(list.files(
    directorio_pasadas, pattern = "[.]parquet$", full.names = TRUE
  ))
  archivos_calidad_entrada <- sort(list.files(
    directorio_calidad, pattern = "[.]parquet$", full.names = TRUE
  ))
  if (!length(archivos_pasadas_entrada) ||
      length(archivos_pasadas_entrada) != length(archivos_calidad_entrada)) {
    stop("Las particiones promovidas del paso 12 estan incompletas.",
      call. = FALSE)
  }
  calidad_horarios <- data.table::rbindlist(lapply(
    archivos_calidad_entrada,
    function(archivo) data.table::as.data.table(arrow::read_parquet(
      archivo,
      col_select = tidyselect::all_of(c(
        "recorrido", "id_version_variante", "tipo_servicio",
        "estado_horario"
      ))
    ))
  ))
  if (!nrow(calidad_horarios) || anyNA(calidad_horarios$recorrido) ||
      anyDuplicated(calidad_horarios$recorrido)) {
    stop("La calidad del paso 12 no identifica recorridos unicos.",
      call. = FALSE)
  }
  calidad_horarios[, `:=`(
    recorrido = as.character(recorrido),
    id_version_variante = as.character(id_version_variante),
    tipo_servicio = as.character(tipo_servicio),
    estado_horario = as.character(estado_horario)
  )]
  aprobados <- calidad_horarios[estado_horario == "horario_completo"]
  cuarentena_horarios <- calidad_horarios[
    estado_horario != "horario_completo"
  ]
  if (!nrow(aprobados)) {
    stop("El paso 12 no contiene recorridos aprobados.", call. = FALSE)
  }

  recorridos <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso11,
    col_select = tidyselect::all_of(c(
      "recorrido", "id_version_variante", "tipo_servicio",
      "registros_utilizables", "personas_utilizables",
      "registros_recuperados"
    ))
  ))
  recorridos[, `:=`(
    recorrido = as.character(recorrido),
    id_version_variante = as.character(id_version_variante),
    tipo_servicio = as.character(tipo_servicio),
    registros_utilizables = as.integer(registros_utilizables),
    personas_utilizables = as.numeric(personas_utilizables),
    registros_recuperados = as.integer(registros_recuperados)
  )]
  if (anyNA(recorridos$recorrido) || anyDuplicated(recorridos$recorrido) ||
      !setequal(recorridos$recorrido, calidad_horarios$recorrido)) {
    stop("Los recorridos del paso 11 no coinciden con la calidad del paso 12.",
      call. = FALSE)
  }
  catalogo_aprobado <- merge(
    recorridos,
    aprobados[, .(
      recorrido,
      id_version_variante_horario = id_version_variante,
      tipo_servicio_horario = tipo_servicio
    )],
    by = "recorrido", all = FALSE, sort = FALSE
  )
  if (nrow(catalogo_aprobado) != nrow(aprobados) ||
      any(catalogo_aprobado$id_version_variante !=
        catalogo_aprobado$id_version_variante_horario) ||
      any(catalogo_aprobado$tipo_servicio !=
        catalogo_aprobado$tipo_servicio_horario)) {
    stop("El catalogo aprobado no coincide entre los pasos 11 y 12.",
      call. = FALSE)
  }

  duracion_recorridos <- data.table::rbindlist(lapply(
    archivos_pasadas_entrada,
    function(archivo) {
      parte <- data.table::as.data.table(arrow::read_parquet(
        archivo,
        col_select = tidyselect::all_of(c("recorrido", "tiempo"))
      ))
      parte[, .(
        inicio_tiempo = min(tiempo),
        fin_tiempo = max(tiempo),
        duracion_h = (max(tiempo) - min(tiempo)) / 3600,
        posiciones = .N
      ), by = recorrido]
    }
  ))
  if (anyDuplicated(duracion_recorridos$recorrido) ||
      !setequal(duracion_recorridos$recorrido, aprobados$recorrido)) {
    stop("Las pasadas no coinciden con los recorridos horarios aprobados.",
      call. = FALSE)
  }
  cuarentena_duracion <- merge(
    duracion_recorridos[
      !is.finite(duracion_h) | duracion_h > 36
    ],
    catalogo_aprobado[, .(
      recorrido, id_version_variante, tipo_servicio
    )],
    by = "recorrido", all.x = TRUE, sort = FALSE
  )
  cuarentena_duracion[, motivo_cuarentena :=
    "duracion_recorrido_mayor_36h"]
  catalogo_procesable <- catalogo_aprobado[
    !recorrido %in% cuarentena_duracion$recorrido
  ]
  if (!nrow(catalogo_procesable)) {
    stop("La cuarentena de duracion elimino todos los recorridos aprobados.",
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
    stop("La OD operativa no tiene trip_id completo y unico.", call. = FALSE)
  }
  od[, `:=`(
    cod_empresa = normalizar_clave_ocupacion(cod_empresa),
    numero_evento_real_subida = normalizar_clave_ocupacion(
      numero_evento_real_subida
    ),
    cantidad_pasajeros = as.numeric(cantidad_pasajeros),
    tipo_servicio = as.character(tipo_servicio),
    tiempo_subida = tiempo_local_ocupacion(
      fecha_evento_subida, zona_horaria
    ),
    tiempo_bajada = tiempo_local_ocupacion(
      fecha_evento_bajada, zona_horaria
    )
  )]
  if (any(!is.finite(od$cantidad_pasajeros)) ||
      any(od$cantidad_pasajeros < 0) ||
      any(od$cantidad_pasajeros != floor(od$cantidad_pasajeros))) {
    stop("La OD tiene cantidades de pasajeros invalidas.", call. = FALSE)
  }
  od[, recorrido := data.table::fifelse(
    !is.na(cod_empresa) & !is.na(numero_evento_real_subida),
    paste(fecha, cod_empresa, numero_evento_real_subida, sep = "|"),
    NA_character_
  )]

  paso09 <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso09,
    col_select = tidyselect::all_of(c(
      "trip_id", "id_version_variante", "indice_secuencia_subida",
      "indice_secuencia_bajada", "punto_canonico_subida",
      "punto_canonico_bajada", "estado_resolucion"
    ))
  ))
  paso10 <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso10,
    col_select = tidyselect::all_of(c(
      "trip_id", "id_version_variante_subida", "indice_secuencia_subida",
      "punto_canonico_subida", "indice_secuencia_bajada_asignado",
      "punto_canonico_bajada_asignado", "recuperada",
      "requiere_revision", "accion_resolucion"
    ))
  ))
  if (anyNA(paso09$trip_id) || anyDuplicated(paso09$trip_id) ||
      !setequal(paso09$trip_id, od$trip_id) ||
      anyNA(paso10$trip_id) || anyDuplicated(paso10$trip_id) ||
      !all(paso10$trip_id %in% paso09$trip_id)) {
    stop("Los pasos 09 o 10 tienen claves trip_id invalidas.", call. = FALSE)
  }
  paso09[, `:=`(
    id_version_variante = as.character(id_version_variante),
    indice_secuencia_subida = as.integer(indice_secuencia_subida),
    indice_secuencia_bajada = as.integer(indice_secuencia_bajada),
    punto_canonico_subida = as.character(punto_canonico_subida),
    punto_canonico_bajada = as.character(punto_canonico_bajada),
    estado_resolucion = as.character(estado_resolucion)
  )]
  delta <- paso10[, .(
    trip_id,
    id_version_variante_recuperada = as.character(
      id_version_variante_subida
    ),
    indice_subida_recuperada = as.integer(indice_secuencia_subida),
    indice_bajada_recuperada = as.integer(
      indice_secuencia_bajada_asignado
    ),
    punto_subida_recuperado = as.character(punto_canonico_subida),
    punto_bajada_recuperado = as.character(
      punto_canonico_bajada_asignado
    ),
    recuperada = as.logical(recuperada),
    requiere_revision = as.logical(requiere_revision),
    accion_recuperacion = as.character(accion_resolucion)
  )]
  base <- merge(paso09, od, by = "trip_id", all.x = TRUE, sort = FALSE)
  base <- merge(base, delta, by = "trip_id", all.x = TRUE, sort = FALSE)
  if (nrow(base) != nrow(od) || anyDuplicated(base$trip_id)) {
    stop("La reconstruccion de viajes altero el universo OD.", call. = FALSE)
  }
  base[, `:=`(
    apta_base = estado_resolucion == "apta_secuencia",
    recuperada_paso10 =
      estado_resolucion == "variante_subida_bajada_distinta" &
      !is.na(recuperada) & recuperada == TRUE
  )]
  base[, viaje_elegible := apta_base | recuperada_paso10]
  base[, `:=`(
    id_version_variante_efectiva = data.table::fifelse(
      recuperada_paso10,
      id_version_variante_recuperada,
      data.table::fifelse(apta_base, id_version_variante, NA_character_)
    ),
    indice_subida_efectivo = data.table::fifelse(
      recuperada_paso10,
      indice_subida_recuperada,
      data.table::fifelse(
        apta_base, indice_secuencia_subida, NA_integer_
      )
    ),
    indice_bajada_efectivo = data.table::fifelse(
      recuperada_paso10,
      indice_bajada_recuperada,
      data.table::fifelse(
        apta_base, indice_secuencia_bajada, NA_integer_
      )
    ),
    punto_subida_efectivo = data.table::fifelse(
      recuperada_paso10,
      punto_subida_recuperado,
      data.table::fifelse(apta_base, punto_canonico_subida, NA_character_)
    ),
    punto_bajada_efectivo = data.table::fifelse(
      recuperada_paso10,
      punto_bajada_recuperado,
      data.table::fifelse(apta_base, punto_canonico_bajada, NA_character_)
    )
  )]
  viajes_aprobados <- base[
    viaje_elegible == TRUE & recorrido %in% catalogo_aprobado$recorrido
  ]
  if (viajes_aprobados[, any(
      is.na(id_version_variante_efectiva) |
      is.na(indice_subida_efectivo) |
      is.na(indice_bajada_efectivo) |
      is.na(punto_subida_efectivo) |
      is.na(punto_bajada_efectivo) |
      indice_bajada_efectivo <= indice_subida_efectivo
  )]) {
    stop("Hay viajes aprobados sin posiciones canonicas validas.",
      call. = FALSE)
  }
  if (nrow(viajes_aprobados) !=
      sum(catalogo_aprobado$registros_utilizables) ||
      sum(viajes_aprobados$cantidad_pasajeros) !=
        sum(catalogo_aprobado$personas_utilizables)) {
    stop("Los viajes reconstruidos no coinciden con el catalogo aprobado.",
      call. = FALSE)
  }
  mapa <- catalogo_aprobado[, .(
    recorrido,
    id_version_variante_catalogo = id_version_variante,
    tipo_servicio_catalogo = tipo_servicio
  )]
  viajes_aprobados <- merge(
    viajes_aprobados, mapa,
    by = "recorrido", all.x = TRUE, sort = FALSE
  )
  if (anyNA(viajes_aprobados$id_version_variante_catalogo) ||
      any(viajes_aprobados$id_version_variante_efectiva !=
        viajes_aprobados$id_version_variante_catalogo) ||
      any(viajes_aprobados$tipo_servicio !=
        viajes_aprobados$tipo_servicio_catalogo)) {
    stop("Los viajes no coinciden con el catalogo aprobado.", call. = FALSE)
  }
  cuarentena_duracion_resumen <- viajes_aprobados[
    recorrido %in% cuarentena_duracion$recorrido,
    .(
      registros_od = .N,
      personas = sum(cantidad_pasajeros)
    ),
    by = recorrido
  ]
  cuarentena_duracion <- merge(
    cuarentena_duracion, cuarentena_duracion_resumen,
    by = "recorrido", all.x = TRUE, sort = FALSE
  )
  cuarentena_duracion[is.na(registros_od), registros_od := 0L]
  cuarentena_duracion[is.na(personas), personas := 0]
  viajes_antes_cuarentena <- viajes_aprobados[
    recorrido %in% catalogo_procesable$recorrido
  ]

  es_tiempo_invertido <-
    is.finite(viajes_antes_cuarentena$tiempo_subida) &
    is.finite(viajes_antes_cuarentena$tiempo_bajada) &
    viajes_antes_cuarentena$tiempo_bajada <
      viajes_antes_cuarentena$tiempo_subida
  cuarentena_tiempo <- viajes_antes_cuarentena[
    es_tiempo_invertido,
    .(
      fecha_operativa = fecha,
      trip_id,
      recorrido,
      id_version_variante = id_version_variante_efectiva,
      tipo_servicio = tipo_servicio_catalogo,
      indice_secuencia_subida = indice_subida_efectivo,
      indice_secuencia_bajada = indice_bajada_efectivo,
      punto_canonico_subida = punto_subida_efectivo,
      punto_canonico_bajada = punto_bajada_efectivo,
      cantidad_pasajeros,
      tiempo_subida,
      tiempo_bajada,
      diferencia_s = tiempo_bajada - tiempo_subida,
      recuperada_paso10,
      requiere_revision,
      accion_recuperacion,
      motivo_cuarentena = "tiempo_bajada_anterior_subida"
    )
  ]
  viajes <- viajes_antes_cuarentena[!es_tiempo_invertido]
  if (!nrow(viajes)) {
    stop("La cuarentena temporal elimino todos los viajes aprobados.",
      call. = FALSE)
  }

  configuracion_checkpoint <- list(
    fecha_operativa = fecha,
    version_registro_variantes = unname(metadatos_salida[["version"]]),
    snapshot_referencias = unname(metadatos_salida[["snapshot"]]),
    red_canonica = unname(metadatos_salida[["red"]]),
    zona_horaria = zona_horaria,
    fuente_horarios = normalizePath(
      directorio_horarios_paso12, winslash = "/", mustWork = TRUE
    ),
    cantidad_lotes = length(archivos_pasadas_entrada),
    recorridos_aprobados_paso12 = nrow(catalogo_aprobado),
    recorridos_cuarentena_duracion = nrow(cuarentena_duracion),
    recorridos_procesables = nrow(catalogo_procesable),
    duracion_maxima_h = 36,
    excluir_tiempos_invertidos = TRUE
  )
  archivo_checkpoint <- file.path(checkpoint, "checkpoint.yml")
  if (dir.exists(checkpoint)) {
    if (!file.exists(archivo_checkpoint)) {
      stop("El checkpoint de ocupacion esta incompleto.", call. = FALSE)
    }
    existente <- yaml::read_yaml(archivo_checkpoint)
    campos <- names(configuracion_checkpoint)
    if (!identical(
        as.character(unlist(existente[campos])),
        as.character(unlist(configuracion_checkpoint[campos]))
    )) {
      stop("El checkpoint de ocupacion pertenece a otra configuracion.",
        call. = FALSE)
    }
    directorios_checkpoint <- file.path(
      checkpoint, c("pasadas_ocupacion", "calidad_ocupacion")
    )
    if (any(!dir.exists(directorios_checkpoint))) {
      stop("El checkpoint no conserva sus particiones.", call. = FALSE)
    }
  } else {
    dir.create(file.path(checkpoint, "pasadas_ocupacion"), recursive = TRUE)
    dir.create(file.path(checkpoint, "calidad_ocupacion"), recursive = TRUE)
    yaml::write_yaml(configuracion_checkpoint, archivo_checkpoint)
  }

  for (indice_lote in seq_along(archivos_pasadas_entrada)) {
    nombre_lote <- tools::file_path_sans_ext(basename(
      archivos_pasadas_entrada[[indice_lote]]
    ))
    archivo_pasadas <- file.path(
      checkpoint, "pasadas_ocupacion", paste0(nombre_lote, ".parquet")
    )
    archivo_calidad <- file.path(
      checkpoint, "calidad_ocupacion", paste0(nombre_lote, ".parquet")
    )
    if (file.exists(archivo_pasadas) && file.exists(archivo_calidad)) {
      message(sprintf(
        "[ocupacion] Lote %s/%s reutilizado.",
        indice_lote, length(archivos_pasadas_entrada)
      ))
      next
    }

    pasadas_lote <- data.table::as.data.table(arrow::read_parquet(
      archivos_pasadas_entrada[[indice_lote]]
    ))
    pasadas_lote <- pasadas_lote[
      recorrido %in% catalogo_procesable$recorrido
    ]
    recorridos_lote <- unique(as.character(pasadas_lote$recorrido))
    viajes_antes_lote <- viajes_antes_cuarentena[
      recorrido %in% recorridos_lote
    ]
    cuarentena_lote <- cuarentena_tiempo[recorrido %in% recorridos_lote]
    viajes_lote <- viajes[recorrido %in% recorridos_lote]
    ocupacion_lote <- calcular_ocupacion_lote(pasadas_lote, viajes_lote)

    identidad_lote <- unique(ocupacion_lote[, .(
      recorrido, id_version_variante, tipo_servicio
    )])
    antes_lote <- viajes_antes_lote[, .(
      registros_antes_cuarentena = .N,
      personas_antes_cuarentena = sum(cantidad_pasajeros)
    ), by = recorrido]
    cuarentena_resumen_lote <- cuarentena_lote[, .(
      viajes_cuarentena_tiempo_invertido = .N,
      personas_cuarentena_tiempo_invertido = sum(cantidad_pasajeros)
    ), by = recorrido]
    viajes_resumen_lote <- viajes_lote[, .(
      registros_ocupacion = .N,
      personas_ocupacion = sum(cantidad_pasajeros)
    ), by = recorrido]
    calidad_lote <- ocupacion_lote[, .(
      posiciones = .N,
      subidas = sum(subidas),
      bajadas = sum(bajadas),
      personas_tramo = sum(ocupacion_personas),
      ocupacion_minima = min(ocupacion_personas),
      ocupacion_maxima = max(ocupacion_personas),
      saldo_final = utils::tail(ocupacion_personas, 1L)
    ), by = .(recorrido, id_version_variante, tipo_servicio)]
    calidad_lote <- merge(
      identidad_lote, calidad_lote,
      by = c("recorrido", "id_version_variante", "tipo_servicio"),
      all.x = TRUE, sort = FALSE
    )
    calidad_lote <- merge(
      calidad_lote, antes_lote, by = "recorrido", all.x = TRUE, sort = FALSE
    )
    calidad_lote <- merge(
      calidad_lote, cuarentena_resumen_lote,
      by = "recorrido", all.x = TRUE, sort = FALSE
    )
    calidad_lote <- merge(
      calidad_lote, viajes_resumen_lote,
      by = "recorrido", all.x = TRUE, sort = FALSE
    )
    columnas_cero <- c(
      "registros_antes_cuarentena", "personas_antes_cuarentena",
      "viajes_cuarentena_tiempo_invertido",
      "personas_cuarentena_tiempo_invertido", "registros_ocupacion",
      "personas_ocupacion"
    )
    for (columna in columnas_cero) {
      data.table::set(
        calidad_lote,
        which(is.na(calidad_lote[[columna]])),
        columna,
        0
      )
    }
    data.table::setcolorder(
      calidad_lote, names(tabla_calidad_ocupacion_vacia())
    )
    if (!nrow(calidad_lote)) {
      calidad_lote <- tabla_calidad_ocupacion_vacia()
    }

    temporal_pasadas <- paste0(archivo_pasadas, ".tmp")
    temporal_calidad <- paste0(archivo_calidad, ".tmp")
    if (!file.exists(archivo_pasadas)) {
      arrow::write_parquet(
        ocupacion_lote, temporal_pasadas, compression = "zstd"
      )
      if (!file.rename(temporal_pasadas, archivo_pasadas)) {
        stop("No se pudo promover un lote de ocupacion.", call. = FALSE)
      }
    }
    if (!file.exists(archivo_calidad)) {
      arrow::write_parquet(
        calidad_lote, temporal_calidad, compression = "zstd"
      )
      if (!file.rename(temporal_calidad, archivo_calidad)) {
        stop("No se pudo promover un lote de calidad de ocupacion.",
          call. = FALSE)
      }
    }
    message(sprintf(
      "[ocupacion] Lote %s/%s terminado.",
      indice_lote, length(archivos_pasadas_entrada)
    ))
  }

  archivos_pasadas_salida <- sort(list.files(
    file.path(checkpoint, "pasadas_ocupacion"),
    pattern = "[.]parquet$", full.names = TRUE
  ))
  archivos_calidad_salida <- sort(list.files(
    file.path(checkpoint, "calidad_ocupacion"),
    pattern = "[.]parquet$", full.names = TRUE
  ))
  cantidad_lotes <- length(archivos_pasadas_entrada)
  if (length(archivos_pasadas_salida) != cantidad_lotes ||
      length(archivos_calidad_salida) != cantidad_lotes ||
      !identical(
        basename(archivos_pasadas_salida),
        basename(archivos_pasadas_entrada)
      )) {
    stop("No se completaron todos los lotes de ocupacion.", call. = FALSE)
  }
  calidad <- data.table::rbindlist(lapply(
    archivos_calidad_salida,
    function(x) data.table::as.data.table(arrow::read_parquet(x))
  ))
  if (nrow(calidad) != nrow(catalogo_procesable) ||
      anyDuplicated(calidad$recorrido)) {
    stop("La calidad no conserva un registro por recorrido aprobado.",
      call. = FALSE)
  }

  filas_entrada <- 0
  filas_salida <- 0
  duplicados <- FALSE
  faltantes_salida <- FALSE
  recorridos_salida <- character()
  for (indice_lote in seq_len(cantidad_lotes)) {
    entrada_lote <- data.table::as.data.table(arrow::read_parquet(
      archivos_pasadas_entrada[[indice_lote]],
      col_select = tidyselect::all_of(c("recorrido", "indice_secuencia"))
    ))[recorrido %in% catalogo_procesable$recorrido]
    salida_lote <- data.table::as.data.table(arrow::read_parquet(
      archivos_pasadas_salida[[indice_lote]],
      col_select = tidyselect::all_of(c(
        "recorrido", "indice_secuencia", "subidas", "bajadas",
        "ocupacion_personas", "tipo_servicio"
      ))
    ))
    filas_entrada <- filas_entrada + nrow(entrada_lote)
    filas_salida <- filas_salida + nrow(salida_lote)
    duplicados <- duplicados || anyDuplicated(
      salida_lote[, .(recorrido, indice_secuencia)]
    ) > 0L
    faltantes_salida <- faltantes_salida ||
      anyNA(salida_lote[, .(
        recorrido, indice_secuencia, tipo_servicio,
        subidas, bajadas, ocupacion_personas
      )]) || any(!is.finite(salida_lote$subidas)) ||
      any(!is.finite(salida_lote$bajadas)) ||
      any(!is.finite(salida_lote$ocupacion_personas))
    recorridos_salida <- c(recorridos_salida, unique(salida_lote$recorrido))
  }

  resumen_base <- calidad[, .(
    recorridos = .N,
    posiciones = sum(posiciones),
    registros_antes_cuarentena = sum(registros_antes_cuarentena),
    personas_antes_cuarentena = sum(personas_antes_cuarentena),
    viajes_cuarentena_tiempo_invertido = sum(
      viajes_cuarentena_tiempo_invertido
    ),
    personas_cuarentena_tiempo_invertido = sum(
      personas_cuarentena_tiempo_invertido
    ),
    registros_ocupacion = sum(registros_ocupacion),
    personas_ocupacion = sum(personas_ocupacion),
    personas_tramo = sum(personas_tramo),
    subidas = sum(subidas),
    bajadas = sum(bajadas),
    ocupacion_maxima = max(ocupacion_maxima)
  ), by = tipo_servicio]
  resumen_total <- calidad[, .(
    tipo_servicio = "total",
    recorridos = .N,
    posiciones = sum(posiciones),
    registros_antes_cuarentena = sum(registros_antes_cuarentena),
    personas_antes_cuarentena = sum(personas_antes_cuarentena),
    viajes_cuarentena_tiempo_invertido = sum(
      viajes_cuarentena_tiempo_invertido
    ),
    personas_cuarentena_tiempo_invertido = sum(
      personas_cuarentena_tiempo_invertido
    ),
    registros_ocupacion = sum(registros_ocupacion),
    personas_ocupacion = sum(personas_ocupacion),
    personas_tramo = sum(personas_tramo),
    subidas = sum(subidas),
    bajadas = sum(bajadas),
    ocupacion_maxima = max(ocupacion_maxima)
  )]
  resumen <- data.table::rbindlist(
    list(resumen_base, resumen_total), use.names = TRUE
  )[order(tipo_servicio)]

  personas_esperadas <- sum(viajes$cantidad_pasajeros)
  personas_tramo_esperadas <- viajes[, sum(
    as.numeric(indice_bajada_efectivo - indice_subida_efectivo) *
      cantidad_pasajeros
  )]
  recorridos_salida_unicos <- unique(recorridos_salida)
  controles <- data.table::data.table(
    control = c(
      "entregas_canonicas_coinciden",
      "calidad_horarios_unica",
      "catalogo_aprobado_coincide",
      "viajes_reconstruidos_coinciden",
      "cuarentena_duracion_aplicada",
      "cuarentena_tiempos_invertidos_aplicada",
      "viajes_incompletos_conservados",
      "particiones_completas",
      "filas_pasadas_conservadas",
      "pasadas_sin_duplicados",
      "pasadas_completas",
      "solo_recorridos_aprobados",
      "subidas_conservan_personas",
      "bajadas_conservan_personas",
      "ocupacion_no_negativa",
      "saldo_final_cero",
      "personas_tramo_coinciden"
    ),
    aprobado = c(
      metadatos_coinciden,
      anyDuplicated(calidad_horarios$recorrido) == 0L,
      nrow(catalogo_aprobado) == nrow(aprobados),
      nrow(viajes_aprobados) ==
        sum(catalogo_aprobado$registros_utilizables) &&
        sum(viajes_aprobados$cantidad_pasajeros) ==
          sum(catalogo_aprobado$personas_utilizables),
      !any(cuarentena_duracion$recorrido %in% recorridos_salida_unicos) &&
        setequal(
          recorridos_salida_unicos,
          catalogo_procesable$recorrido
        ),
      !any(
        is.finite(viajes$tiempo_subida) &
        is.finite(viajes$tiempo_bajada) &
        viajes$tiempo_bajada < viajes$tiempo_subida
      ),
      sum(!is.finite(viajes$tiempo_subida) |
        !is.finite(viajes$tiempo_bajada)) ==
        sum(!is.finite(viajes_antes_cuarentena$tiempo_subida) |
          !is.finite(viajes_antes_cuarentena$tiempo_bajada)),
      length(archivos_pasadas_salida) == cantidad_lotes &&
        length(archivos_calidad_salida) == cantidad_lotes,
      filas_entrada == filas_salida,
      !duplicados,
      !faltantes_salida,
      setequal(
        recorridos_salida_unicos,
        catalogo_procesable$recorrido
      ) &&
        !any(cuarentena_horarios$recorrido %in% recorridos_salida_unicos),
      sum(calidad$subidas) == personas_esperadas,
      sum(calidad$bajadas) == personas_esperadas,
      all(calidad$ocupacion_minima >= 0),
      all(calidad$saldo_final == 0),
      sum(calidad$personas_tramo) == personas_tramo_esperadas
    )
  )
  controles[, estado := data.table::fifelse(
    aprobado, "aprobado", "fallo"
  )]
  if (any(!controles$aprobado)) {
    stop("La ocupacion no aprobo sus controles.", call. = FALSE)
  }

  arrow::write_parquet(
    cuarentena_tiempo,
    file.path(checkpoint, "viajes_cuarentena_tiempo_invertido.parquet"),
    compression = "zstd"
  )
  arrow::write_parquet(
    cuarentena_duracion,
    file.path(checkpoint, "recorridos_cuarentena_duracion.parquet"),
    compression = "zstd"
  )
  data.table::fwrite(
    resumen, file.path(checkpoint, "resumen_ocupacion.csv")
  )
  data.table::fwrite(controles, file.path(checkpoint, "controles.csv"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    tipo_salida = "ocupacion_recorridos_particionada",
    fuente_horarios = normalizePath(
      directorio_horarios_paso12, winslash = "/", mustWork = TRUE
    ),
    version_registro_variantes = metadatos_salida[["version"]],
    snapshot_referencias = metadatos_salida[["snapshot"]],
    red_canonica = metadatos_salida[["red"]],
    zona_horaria = zona_horaria,
    cantidad_lotes = cantidad_lotes,
    recorridos_aprobados_paso12 = nrow(catalogo_aprobado),
    recorridos_cuarentena_duracion = nrow(cuarentena_duracion),
    recorridos = nrow(calidad),
    filas_pasadas = filas_salida,
    registros_antes_cuarentena = nrow(viajes_antes_cuarentena),
    personas_antes_cuarentena = sum(
      viajes_antes_cuarentena$cantidad_pasajeros
    ),
    viajes_cuarentena_tiempo_invertido = nrow(cuarentena_tiempo),
    personas_cuarentena_tiempo_invertido = sum(
      cuarentena_tiempo$cantidad_pasajeros
    ),
    registros_ocupacion = nrow(viajes),
    personas_ocupacion = personas_esperadas,
    personas_tramo = sum(calidad$personas_tramo),
    subidas = sum(calidad$subidas),
    bajadas = sum(calidad$bajadas),
    incluye_tabla_viajes = FALSE,
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(checkpoint, "manifiesto.yml"))
  if (!file.rename(checkpoint, destino)) {
    stop(sprintf("No se pudo promover la salida de ocupacion: %s", destino),
      call. = FALSE)
  }
  list(
    destino = destino,
    calidad = calidad,
    cuarentena_duracion = cuarentena_duracion,
    cuarentena_tiempo = cuarentena_tiempo,
    resumen = resumen,
    controles = controles
  )
}
