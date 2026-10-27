normalizar_clave_recuperacion <- function(x) {
  valor <- trimws(as.character(x))
  valor[is.na(x) | !nzchar(valor) | valor == "NA"] <- NA_character_
  valor
}

distancia_haversine_m <- function(lon_origen, lat_origen, lon_destino, lat_destino) {
  radianes <- pi / 180
  delta_lat <- (lat_destino - lat_origen) * radianes
  delta_lon <- (lon_destino - lon_origen) * radianes
  a <- sin(delta_lat / 2)^2 +
    cos(lat_origen * radianes) * cos(lat_destino * radianes) *
      sin(delta_lon / 2)^2
  2 * 6371000 * asin(pmin(1, sqrt(a)))
}

recuperar_paradas_variantes_distintas <- function(
    fecha,
    archivo_od_operativa,
    archivo_resolucion_paso09,
    directorio_uso_variantes,
    directorio_registro_canonico,
    directorio_salida,
    distancia_maxima_m = 500) {
  for (paquete in c("arrow", "data.table", "tidyselect", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  fecha_date <- as.Date(fecha)
  if (length(fecha) != 1L || is.na(fecha_date) ||
      format(fecha_date, "%Y-%m-%d") != fecha) {
    stop("La fecha debe tener formato AAAA-MM-DD.", call. = FALSE)
  }
  if (length(distancia_maxima_m) != 1L || !is.finite(distancia_maxima_m) ||
      distancia_maxima_m <= 0) {
    stop("La distancia maxima debe ser un numero positivo.", call. = FALSE)
  }

  destino <- file.path(
    directorio_salida, "recuperacion_paradas", paste0("fecha=", fecha)
  )
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf(
      "La recuperacion diaria ya existe y no sera sobrescrita: %s", destino
    ), call. = FALSE)
  }

  archivo_uso <- file.path(directorio_uso_variantes, "uso_variantes.parquet")
  manifiesto_uso_archivo <- file.path(directorio_uso_variantes, "manifiesto.yml")
  manifiesto_paso09_archivo <- file.path(
    dirname(archivo_resolucion_paso09), "manifiesto.yml"
  )
  archivo_paradas <- file.path(
    directorio_registro_canonico, "paradas_proyectadas.parquet"
  )
  manifiesto_registro_archivo <- file.path(
    directorio_registro_canonico, "manifiesto.yml"
  )
  entradas <- c(
    archivo_od_operativa, archivo_resolucion_paso09,
    manifiesto_paso09_archivo, archivo_uso, manifiesto_uso_archivo,
    archivo_paradas, manifiesto_registro_archivo
  )
  faltantes <- entradas[!file.exists(entradas)]
  if (length(faltantes)) {
    stop(sprintf(
      "Faltan archivos de entrada: %s", paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  manifiesto_paso09 <- yaml::read_yaml(manifiesto_paso09_archivo)
  manifiesto_uso <- yaml::read_yaml(manifiesto_uso_archivo)
  manifiesto_registro <- yaml::read_yaml(manifiesto_registro_archivo)
  if (!identical(as.character(manifiesto_paso09$fecha_operativa), fecha) ||
      !identical(as.character(manifiesto_uso$fecha_operativa), fecha)) {
    stop("Las entradas diarias no corresponden a la fecha solicitada.", call. = FALSE)
  }
  if (!identical(manifiesto_registro$etapa, "registro_canonico") ||
      !identical(manifiesto_registro$estado, "completo")) {
    stop("El registro canonico no esta promovido o esta incompleto.", call. = FALSE)
  }
  metadatos_paso09 <- c(
    version = manifiesto_paso09$version_registro_variantes,
    snapshot = manifiesto_paso09$snapshot_referencias,
    red = manifiesto_paso09$red_canonica
  )
  metadatos_uso <- c(
    version = manifiesto_uso$version_registro_variantes,
    snapshot = manifiesto_uso$snapshot_referencias,
    red = manifiesto_uso$red_canonica
  )
  metadatos_registro <- c(
    version = manifiesto_registro$version,
    snapshot = manifiesto_registro$snapshot_referencias,
    red = manifiesto_registro$red_canonica
  )
  metadatos_coinciden <-
    identical(as.character(metadatos_paso09), as.character(metadatos_uso)) &&
    identical(as.character(metadatos_uso), as.character(metadatos_registro))
  if (!metadatos_coinciden || anyNA(metadatos_registro)) {
    stop("Los pasos 08, 09 y el registro no usan la misma entrega.", call. = FALSE)
  }

  columnas_resolucion <- c(
    "trip_id", "codigo_variante_subida", "codigo_variante_bajada",
    "codigo_parada_subida", "codigo_parada_bajada", "estado_resolucion"
  )
  resolucion <- data.table::as.data.table(arrow::read_parquet(
    archivo_resolucion_paso09,
    col_select = tidyselect::all_of(columnas_resolucion)
  ))
  resolucion <- resolucion[
    estado_resolucion == "variante_subida_bajada_distinta"
  ]
  if (anyNA(resolucion$trip_id) || anyDuplicated(resolucion$trip_id)) {
    stop("El paso 09 no tiene un trip_id unico en la cuarentena.", call. = FALSE)
  }
  resolucion[, `:=`(
    codigo_variante_subida = normalizar_clave_recuperacion(codigo_variante_subida),
    codigo_variante_bajada = normalizar_clave_recuperacion(codigo_variante_bajada),
    codigo_parada_subida = normalizar_clave_recuperacion(codigo_parada_subida),
    codigo_parada_bajada = normalizar_clave_recuperacion(codigo_parada_bajada)
  )]

  columnas_od <- c(
    "trip_id", "linea_codigo_subida", "linea_codigo_bajada",
    "cantidad_pasajeros", "tipo_servicio"
  )
  od <- data.table::as.data.table(arrow::read_parquet(
    archivo_od_operativa,
    col_select = tidyselect::all_of(columnas_od)
  ))
  if (anyNA(od$trip_id) || anyDuplicated(od$trip_id)) {
    stop("La OD operativa no tiene un trip_id completo y unico.", call. = FALSE)
  }
  od[, `:=`(
    linea_codigo_subida = normalizar_clave_recuperacion(linea_codigo_subida),
    linea_codigo_bajada = normalizar_clave_recuperacion(linea_codigo_bajada),
    cantidad_pasajeros = as.numeric(cantidad_pasajeros),
    tipo_servicio = as.character(tipo_servicio)
  )]
  base <- merge(
    resolucion,
    od[, .(
      trip_id, linea_codigo_subida, linea_codigo_bajada,
      cantidad_pasajeros, tipo_servicio
    )],
    by = "trip_id", all.x = TRUE, sort = FALSE
  )
  if (nrow(base) != nrow(resolucion) || anyNA(base$tipo_servicio)) {
    stop("La union con la OD no conservo la cuarentena del paso 09.", call. = FALSE)
  }
  base[, misma_linea :=
    !is.na(linea_codigo_subida) & !is.na(linea_codigo_bajada) &
      linea_codigo_subida == linea_codigo_bajada]

  uso <- data.table::as.data.table(arrow::read_parquet(
    archivo_uso,
    col_select = tidyselect::all_of(c(
      "codigo_variante", "id_version_variante", "estado_resolucion"
    ))
  ))
  uso[, `:=`(
    codigo_variante = normalizar_clave_recuperacion(codigo_variante),
    id_version_variante = as.character(id_version_variante),
    estado_resolucion = as.character(estado_resolucion)
  )]
  if (anyDuplicated(uso$codigo_variante)) {
    stop("El paso 08 contiene variantes duplicadas.", call. = FALSE)
  }
  mapa_subida <- uso[, .(
    codigo_variante_subida = codigo_variante,
    id_version_variante_subida = id_version_variante,
    estado_variante_subida = estado_resolucion
  )]
  mapa_bajada <- uso[, .(
    codigo_variante_bajada = codigo_variante,
    id_version_variante_bajada = id_version_variante,
    estado_variante_bajada = estado_resolucion
  )]
  base <- merge(
    base, mapa_subida,
    by = "codigo_variante_subida", all.x = TRUE, sort = FALSE
  )
  base <- merge(
    base, mapa_bajada,
    by = "codigo_variante_bajada", all.x = TRUE, sort = FALSE
  )
  if (nrow(base) != nrow(resolucion) || anyDuplicated(base$trip_id)) {
    stop("La union con las versiones altero la cuarentena.", call. = FALSE)
  }

  columnas_paradas <- c(
    "id_version_variante", "cod_ubic_parada", "indice_secuencia",
    "punto_canonico_id", "x", "y"
  )
  paradas <- data.table::as.data.table(arrow::read_parquet(
    archivo_paradas,
    col_select = tidyselect::all_of(columnas_paradas)
  ))
  paradas[, `:=`(
    id_version_variante = as.character(id_version_variante),
    codigo_parada = normalizar_clave_recuperacion(cod_ubic_parada),
    indice_secuencia = as.integer(indice_secuencia),
    punto_canonico_id = as.character(punto_canonico_id),
    x = as.numeric(x),
    y = as.numeric(y)
  )]
  paradas[, cod_ubic_parada := NULL]
  paradas <- unique(paradas)
  if (anyDuplicated(
      paradas,
      by = c("id_version_variante", "codigo_parada", "indice_secuencia")
  )) {
    stop("El registro tiene posiciones de parada ambiguas.", call. = FALSE)
  }

  resultado <- base[, .(
    fecha_operativa = fecha,
    trip_id,
    linea_codigo_subida,
    linea_codigo_bajada,
    codigo_variante_subida,
    codigo_variante_bajada,
    id_version_variante_subida,
    id_version_variante_bajada,
    codigo_parada_subida_original = codigo_parada_subida,
    codigo_parada_bajada_original = codigo_parada_bajada,
    indice_secuencia_subida = NA_integer_,
    punto_canonico_subida = NA_character_,
    codigo_parada_bajada_candidata = NA_character_,
    indice_secuencia_bajada_candidata = NA_integer_,
    punto_canonico_bajada_candidato = NA_character_,
    codigo_parada_bajada_asignada = NA_character_,
    indice_secuencia_bajada_asignado = NA_integer_,
    punto_canonico_bajada_asignado = NA_character_,
    distancia_recuperacion_m = NA_real_,
    distancia_maxima_m = as.numeric(distancia_maxima_m),
    coordenadas_objetivo = NA_integer_,
    recuperada = FALSE,
    requiere_revision = FALSE,
    accion_resolucion = data.table::fcase(
      !misma_linea, "linea_distinta",
      is.na(id_version_variante_subida) |
        estado_variante_subida != "resuelta",
      "variante_subida_no_resuelta",
      default = "pendiente_resolucion"
    )
  )]

  elegibles <- base[
    misma_linea & estado_variante_subida == "resuelta" &
      !is.na(id_version_variante_subida)
  ]
  origen_ref <- paradas[, .(
    id_version_variante_subida = id_version_variante,
    codigo_parada_subida = codigo_parada,
    indice_secuencia_subida = indice_secuencia,
    punto_canonico_subida = punto_canonico_id
  )]
  origenes <- merge(
    elegibles,
    origen_ref,
    by = c("id_version_variante_subida", "codigo_parada_subida"),
    all = FALSE, allow.cartesian = TRUE, sort = FALSE
  )
  ids_sin_origen <- setdiff(elegibles$trip_id, unique(origenes$trip_id))
  resultado[
    trip_id %in% ids_sin_origen & accion_resolucion == "pendiente_resolucion",
    accion_resolucion := "origen_no_resuelto"
  ]
  origen_preferido <- origenes[
    order(trip_id, indice_secuencia_subida)
  ][, .SD[1L], by = trip_id]
  resultado[origen_preferido, on = "trip_id", `:=`(
    indice_secuencia_subida = i.indice_secuencia_subida,
    punto_canonico_subida = i.punto_canonico_subida
  )]

  destino_ref <- paradas[, .(
    id_version_variante_subida = id_version_variante,
    codigo_parada_bajada = codigo_parada,
    indice_secuencia_bajada_candidata = indice_secuencia,
    punto_canonico_bajada_candidato = punto_canonico_id
  )]
  pares_exactos <- merge(
    origenes,
    destino_ref,
    by = c("id_version_variante_subida", "codigo_parada_bajada"),
    all = FALSE, allow.cartesian = TRUE, sort = FALSE
  )
  pares_exactos <- pares_exactos[
    indice_secuencia_bajada_candidata > indice_secuencia_subida
  ]
  pares_exactos[, tramo :=
    indice_secuencia_bajada_candidata - indice_secuencia_subida]
  data.table::setorder(
    pares_exactos, trip_id, tramo,
    indice_secuencia_subida, indice_secuencia_bajada_candidata
  )
  exactos <- pares_exactos[, .SD[1L], by = trip_id]
  resultado[exactos, on = "trip_id", `:=`(
    indice_secuencia_subida = i.indice_secuencia_subida,
    punto_canonico_subida = i.punto_canonico_subida,
    codigo_parada_bajada_candidata = i.codigo_parada_bajada,
    indice_secuencia_bajada_candidata = i.indice_secuencia_bajada_candidata,
    punto_canonico_bajada_candidato = i.punto_canonico_bajada_candidato,
    codigo_parada_bajada_asignada = i.codigo_parada_bajada,
    indice_secuencia_bajada_asignado = i.indice_secuencia_bajada_candidata,
    punto_canonico_bajada_asignado = i.punto_canonico_bajada_candidato,
    distancia_recuperacion_m = 0,
    recuperada = TRUE,
    requiere_revision = FALSE,
    accion_resolucion = "parada_exacta_en_variante_subida"
  )]

  ids_pendientes <- resultado[
    accion_resolucion == "pendiente_resolucion", trip_id
  ]
  if (length(ids_pendientes)) {
    objetivos_ref <- paradas[
      is.finite(x) & is.finite(y),
      .(
        x_objetivo = stats::median(x),
        y_objetivo = stats::median(y),
        coordenadas_objetivo = data.table::uniqueN(paste(
          round(x, 7), round(y, 7), sep = "|"
        ))
      ),
      by = .(
        id_version_variante_bajada = id_version_variante,
        codigo_parada_bajada = codigo_parada
      )
    ]
    objetivos <- merge(
      origenes[
        trip_id %in% ids_pendientes &
          !is.na(id_version_variante_bajada) &
          !is.na(estado_variante_bajada) &
          estado_variante_bajada == "resuelta"
      ],
      objetivos_ref,
      by = c("id_version_variante_bajada", "codigo_parada_bajada"),
      all.x = TRUE, allow.cartesian = TRUE, sort = FALSE
    )
    coordenadas_por_viaje <- objetivos[
      order(trip_id, indice_secuencia_subida)
    ][, .SD[1L], by = trip_id]
    resultado[coordenadas_por_viaje, on = "trip_id", `:=`(
      coordenadas_objetivo = i.coordenadas_objetivo
    )]

    ids_sin_version_bajada <- base[
      trip_id %in% ids_pendientes &
        (is.na(id_version_variante_bajada) |
          is.na(estado_variante_bajada) |
          estado_variante_bajada != "resuelta"),
      trip_id
    ]
    resultado[
      trip_id %in% ids_sin_version_bajada &
        accion_resolucion == "pendiente_resolucion",
      accion_resolucion := "variante_bajada_no_resuelta"
    ]
    ids_sin_coordenadas <- setdiff(
      ids_pendientes,
      unique(objetivos[is.finite(x_objetivo) & is.finite(y_objetivo), trip_id])
    )
    resultado[
      trip_id %in% ids_sin_coordenadas &
        accion_resolucion == "pendiente_resolucion",
      accion_resolucion := "sin_coordenadas_destino"
    ]

    objetivos_validos <- objetivos[
      is.finite(x_objetivo) & is.finite(y_objetivo)
    ]
    candidatos_ref <- paradas[
      is.finite(x) & is.finite(y),
      .(
        id_version_variante_subida = id_version_variante,
        codigo_parada_bajada_candidata = codigo_parada,
        indice_secuencia_bajada_candidata = indice_secuencia,
        punto_canonico_bajada_candidato = punto_canonico_id,
        x_candidata = x,
        y_candidata = y
      )
    ]
    candidatos <- merge(
      objetivos_validos,
      candidatos_ref,
      by = "id_version_variante_subida",
      all = FALSE, allow.cartesian = TRUE, sort = FALSE
    )
    candidatos <- candidatos[
      indice_secuencia_bajada_candidata > indice_secuencia_subida
    ]
    candidatos[, distancia_recuperacion_m := distancia_haversine_m(
      x_objetivo, y_objetivo, x_candidata, y_candidata
    )]
    candidatos[, tramo :=
      indice_secuencia_bajada_candidata - indice_secuencia_subida]
    data.table::setorder(
      candidatos, trip_id, distancia_recuperacion_m, tramo,
      indice_secuencia_subida, indice_secuencia_bajada_candidata,
      codigo_parada_bajada_candidata
    )
    mejores <- candidatos[, .SD[1L], by = trip_id]
    resultado[mejores, on = "trip_id", `:=`(
      indice_secuencia_subida = i.indice_secuencia_subida,
      punto_canonico_subida = i.punto_canonico_subida,
      codigo_parada_bajada_candidata = i.codigo_parada_bajada_candidata,
      indice_secuencia_bajada_candidata = i.indice_secuencia_bajada_candidata,
      punto_canonico_bajada_candidato = i.punto_canonico_bajada_candidato,
      distancia_recuperacion_m = i.distancia_recuperacion_m
    )]
    recuperadas_cercania <- mejores[distancia_recuperacion_m <= distancia_maxima_m]
    resultado[recuperadas_cercania, on = "trip_id", `:=`(
      codigo_parada_bajada_asignada = i.codigo_parada_bajada_candidata,
      indice_secuencia_bajada_asignado = i.indice_secuencia_bajada_candidata,
      punto_canonico_bajada_asignado = i.punto_canonico_bajada_candidato,
      recuperada = TRUE,
      requiere_revision = TRUE,
      accion_resolucion = "parada_cercana_en_variante_subida"
    )]
    lejanas <- mejores[distancia_recuperacion_m > distancia_maxima_m]
    resultado[lejanas, on = "trip_id",
      accion_resolucion := "distancia_mayor_umbral"]
    ids_sin_candidata <- setdiff(
      unique(objetivos_validos$trip_id), unique(mejores$trip_id)
    )
    resultado[
      trip_id %in% ids_sin_candidata &
        accion_resolucion == "pendiente_resolucion",
      accion_resolucion := "sin_parada_posterior"
    ]
  }

  resultado[, `:=`(
    version_registro_variantes = as.character(metadatos_registro[["version"]]),
    snapshot_referencias = as.character(metadatos_registro[["snapshot"]]),
    red_canonica = as.character(metadatos_registro[["red"]])
  )]
  data.table::setorder(resultado, trip_id)

  acciones_validas <- c(
    "parada_exacta_en_variante_subida",
    "parada_cercana_en_variante_subida",
    "linea_distinta", "variante_subida_no_resuelta",
    "variante_bajada_no_resuelta", "origen_no_resuelto",
    "sin_coordenadas_destino", "sin_parada_posterior",
    "distancia_mayor_umbral"
  )
  controles <- data.table::data.table(
    control = c(
      "delta_trip_id_unico", "cuarentena_paso09_conservada",
      "entregas_canonicas_coinciden", "acciones_validas",
      "recuperadas_misma_linea", "recuperadas_orden_positivo",
      "recuperadas_bajo_umbral", "exactas_conservan_codigo",
      "cercanas_registran_revision", "no_recuperadas_sin_asignacion",
      "registros_conservados", "personas_conservadas"
    ),
    aprobado = c(
      anyDuplicated(resultado$trip_id) == 0L,
      setequal(resultado$trip_id, resolucion$trip_id),
      metadatos_coinciden,
      all(resultado$accion_resolucion %in% acciones_validas),
      resultado[recuperada == TRUE, all(
        !is.na(linea_codigo_subida) &
          linea_codigo_subida == linea_codigo_bajada
      )],
      resultado[recuperada == TRUE, all(
        indice_secuencia_bajada_asignado > indice_secuencia_subida
      )],
      resultado[recuperada == TRUE, all(
        distancia_recuperacion_m <= distancia_maxima_m
      )],
      resultado[
        accion_resolucion == "parada_exacta_en_variante_subida",
        all(codigo_parada_bajada_asignada == codigo_parada_bajada_original)
      ],
      resultado[
        accion_resolucion == "parada_cercana_en_variante_subida",
        all(requiere_revision)
      ],
      resultado[recuperada == FALSE, all(
        is.na(codigo_parada_bajada_asignada) &
          is.na(indice_secuencia_bajada_asignado) &
          is.na(punto_canonico_bajada_asignado)
      )],
      nrow(resultado) == nrow(base),
      isTRUE(all.equal(
        sum(base$cantidad_pasajeros, na.rm = TRUE),
        sum(base$cantidad_pasajeros[
          match(resultado$trip_id, base$trip_id)
        ], na.rm = TRUE)
      ))
    )
  )
  controles[, estado := data.table::fifelse(aprobado, "aprobado", "fallo")]
  if (any(!controles$aprobado)) {
    stop("La recuperacion de paradas no aprobo sus controles.", call. = FALSE)
  }

  auditoria <- merge(
    resultado[, .(
      trip_id, recuperada, requiere_revision, accion_resolucion,
      codigo_variante_subida, codigo_variante_bajada,
      distancia_recuperacion_m
    )],
    base[, .(trip_id, tipo_servicio, cantidad_pasajeros)],
    by = "trip_id", all.x = TRUE, sort = FALSE
  )
  resumen <- auditoria[, .(
    registros = .N,
    personas = sum(cantidad_pasajeros, na.rm = TRUE),
    pares_variantes = data.table::uniqueN(paste(
      codigo_variante_subida, codigo_variante_bajada, sep = "->"
    )),
    distancia_mediana_m = if (all(is.na(distancia_recuperacion_m))) {
      NA_real_
    } else {
      stats::median(distancia_recuperacion_m, na.rm = TRUE)
    },
    distancia_maxima_observada_m = if (all(is.na(distancia_recuperacion_m))) {
      NA_real_
    } else {
      max(distancia_recuperacion_m, na.rm = TRUE)
    }
  ), by = .(tipo_servicio, recuperada, requiere_revision, accion_resolucion)]
  resumen_total <- auditoria[, .(
    tipo_servicio = "total",
    registros = .N,
    personas = sum(cantidad_pasajeros, na.rm = TRUE),
    pares_variantes = data.table::uniqueN(paste(
      codigo_variante_subida, codigo_variante_bajada, sep = "->"
    )),
    distancia_mediana_m = if (all(is.na(distancia_recuperacion_m))) {
      NA_real_
    } else {
      stats::median(distancia_recuperacion_m, na.rm = TRUE)
    },
    distancia_maxima_observada_m = if (all(is.na(distancia_recuperacion_m))) {
      NA_real_
    } else {
      max(distancia_recuperacion_m, na.rm = TRUE)
    }
  ), by = .(recuperada, requiere_revision, accion_resolucion)]
  resumen <- data.table::rbindlist(
    list(resumen, resumen_total), use.names = TRUE
  )[order(tipo_servicio, -recuperada, accion_resolucion)]

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(
    dirname(destino),
    sprintf(".tmp_fecha_%s_%s", gsub("-", "", fecha), Sys.getpid())
  )
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) {
      unlink(temporal, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  arrow::write_parquet(
    resultado,
    file.path(temporal, "recuperaciones_paradas.parquet"),
    compression = "zstd"
  )
  data.table::fwrite(resumen, file.path(temporal, "resumen_recuperacion.csv"))
  data.table::fwrite(controles, file.path(temporal, "controles.csv"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    tipo_salida = "delta_recuperacion_paradas",
    fuente_paso09 = normalizePath(
      dirname(archivo_resolucion_paso09), winslash = "/", mustWork = TRUE
    ),
    registro_canonico = normalizePath(
      directorio_registro_canonico, winslash = "/", mustWork = TRUE
    ),
    version_registro_variantes = metadatos_registro[["version"]],
    snapshot_referencias = metadatos_registro[["snapshot"]],
    red_canonica = metadatos_registro[["red"]],
    distancia_maxima_m = as.numeric(distancia_maxima_m),
    politica = "misma_linea_exacta_o_cercana_posterior_al_origen",
    filas_evaluadas = nrow(resultado),
    filas_recuperadas = resultado[recuperada == TRUE, .N],
    filas_no_recuperadas = resultado[recuperada == FALSE, .N],
    incluye_od_completa = FALSE,
    incluye_geometrias = FALSE,
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))
  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover el delta de recuperacion: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  list(
    destino = destino,
    recuperaciones = resultado,
    resumen = resumen,
    controles = controles
  )
}
