normalizar_clave_horario <- function(x) {
  valor <- trimws(as.character(x))
  valor[is.na(x) | !nzchar(valor) | valor == "NA"] <- NA_character_
  valor
}

tiempo_local_numerico <- function(x, zona_horaria) {
  texto <- if (inherits(x, "POSIXt")) {
    format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else {
    as.character(x)
  }
  as.numeric(as.POSIXct(
    texto, format = "%Y-%m-%d %H:%M:%S", tz = zona_horaria
  ))
}

preparar_anclas_horario <- function(observaciones) {
  x <- data.table::as.data.table(observaciones)
  anclas <- x[is.finite(distancia_m) & is.finite(tiempo), .(
    tiempo_original = stats::median(tiempo),
    tiempo_minimo = min(tiempo),
    tiempo_maximo = max(tiempo),
    subidas = sum(fuente == "subida"),
    bajadas = sum(fuente == "bajada")
  ), by = distancia_m]
  data.table::setorder(anclas, distancia_m)
  if (!nrow(anclas)) return(anclas)
  anclas[, tiempo := if (.N == 1L) {
    tiempo_original
  } else {
    stats::isoreg(distancia_m, tiempo_original)$yf
  }]
  anclas[, ajuste_s := tiempo - tiempo_original]
  anclas
}

velocidades_borde_horario <- function(anclas) {
  if (nrow(anclas) < 2L) {
    return(c(inicio = NA_real_, final = NA_real_))
  }
  distancia <- diff(anclas$distancia_m)
  tiempo <- diff(anclas$tiempo)
  validas <- distancia > 0 & tiempo > 0
  velocidades <- distancia[validas] / tiempo[validas]
  if (!length(velocidades)) {
    return(c(inicio = NA_real_, final = NA_real_))
  }
  c(
    inicio = stats::median(utils::head(velocidades, 5L)),
    final = stats::median(utils::tail(velocidades, 5L))
  )
}

estimar_tiempos_secuencia <- function(distancias, anclas, velocidad_respaldo) {
  if (length(velocidad_respaldo) != 1L ||
      !is.finite(velocidad_respaldo) || velocidad_respaldo <= 0) {
    stop("La velocidad de respaldo debe ser positiva.", call. = FALSE)
  }
  if (!nrow(anclas)) {
    return(data.table::data.table(
      tiempo = rep(NA_real_, length(distancias)),
      metodo_tiempo = "sin_anclas"
    ))
  }
  velocidades <- velocidades_borde_horario(anclas)
  usa_respaldo <- anyNA(velocidades)
  if (usa_respaldo) velocidades[] <- velocidad_respaldo
  if (usa_respaldo) {
    intercepto <- stats::median(
      anclas$tiempo - anclas$distancia_m / velocidad_respaldo
    )
    return(data.table::data.table(
      tiempo = intercepto + distancias / velocidad_respaldo,
      metodo_tiempo = "proyectado_velocidad_global"
    ))
  }
  tiempo <- stats::approx(
    anclas$distancia_m, anclas$tiempo, xout = distancias, rule = 1
  )$y
  antes <- distancias < min(anclas$distancia_m)
  despues <- distancias > max(anclas$distancia_m)
  tiempo[antes] <- anclas$tiempo[[1L]] +
    (distancias[antes] - anclas$distancia_m[[1L]]) / velocidades[[1L]]
  tiempo[despues] <- utils::tail(anclas$tiempo, 1L) +
    (distancias[despues] - utils::tail(anclas$distancia_m, 1L)) /
      velocidades[[2L]]
  data.table::data.table(
    tiempo = tiempo,
    metodo_tiempo = data.table::fcase(
      antes, "extrapolado_antes",
      despues, "extrapolado_despues",
      distancias %in% anclas$distancia_m, "ancla_resumida",
      default = "interpolado"
    )
  )
}

clasificar_calidad_horario <- function(
    calidad,
    velocidad_maxima_kmh,
    ajuste_maximo_s) {
  x <- data.table::copy(data.table::as.data.table(calidad))
  if (!is.finite(velocidad_maxima_kmh) || velocidad_maxima_kmh <= 0 ||
      !is.finite(ajuste_maximo_s) || ajuste_maximo_s < 0) {
    stop("Los umbrales de calidad temporal son invalidos.", call. = FALSE)
  }
  x[, velocidad_extrema_kmh := pmax(
    velocidad_inicio_kmh, velocidad_final_kmh, na.rm = TRUE
  )]
  x[!is.finite(velocidad_extrema_kmh), velocidad_extrema_kmh := NA_real_]
  x[, motivo_revision_temporal := data.table::fcase(
    velocidad_extrema_kmh > velocidad_maxima_kmh &
      ajuste_max_s > ajuste_maximo_s,
    "velocidad_y_ajuste",
    velocidad_extrema_kmh > velocidad_maxima_kmh, "velocidad",
    ajuste_max_s > ajuste_maximo_s, "ajuste",
    default = NA_character_
  )]
  x[, estado_horario := data.table::fcase(
    posiciones_sin_hora > 0L, "sin_anclas",
    !is.na(motivo_revision_temporal),
    paste0("revision_temporal_", motivo_revision_temporal),
    default = "horario_completo"
  )]
  x
}

tabla_pasadas_vacia <- function() {
  data.table::data.table(
    recorrido = character(),
    id_version_variante = character(),
    tipo_servicio = character(),
    indice_secuencia = integer(),
    punto_canonico_id = character(),
    distancia_m = numeric(),
    tiempo = numeric(),
    metodo_tiempo = character(),
    buses = integer()
  )
}

estimar_horarios_recorridos <- function(
    fecha,
    archivo_od_operativa,
    directorio_resolucion_paso09,
    directorio_recuperacion_paso10,
    directorio_recorridos_paso11,
    directorio_registro_canonico,
    directorio_salida,
    zona_horaria = "America/Montevideo",
    recorridos_por_lote = 500L,
    velocidad_maxima_kmh = 100,
    ajuste_maximo_s = 1800) {
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
  recorridos_por_lote <- as.integer(recorridos_por_lote)
  if (is.na(recorridos_por_lote) || recorridos_por_lote <= 0L) {
    stop("La cantidad de recorridos por lote debe ser positiva.", call. = FALSE)
  }
  if (!is.finite(velocidad_maxima_kmh) || velocidad_maxima_kmh <= 0 ||
      !is.finite(ajuste_maximo_s) || ajuste_maximo_s < 0) {
    stop("Los umbrales de horarios son invalidos.", call. = FALSE)
  }

  padre_salida <- file.path(directorio_salida, "horarios_recorridos")
  destino <- file.path(padre_salida, paste0("fecha=", fecha))
  checkpoint <- file.path(
    padre_salida, paste0(".checkpoint_fecha=", gsub("-", "", fecha))
  )
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf(
      "Los horarios ya existen y no seran sobrescritos: %s", destino
    ), call. = FALSE)
  }

  archivo_paso09 <- file.path(
    directorio_resolucion_paso09, "resolucion_paradas.parquet"
  )
  manifiesto_paso09_archivo <- file.path(
    directorio_resolucion_paso09, "manifiesto.yml"
  )
  archivo_paso10 <- file.path(
    directorio_recuperacion_paso10, "recuperaciones_paradas.parquet"
  )
  manifiesto_paso10_archivo <- file.path(
    directorio_recuperacion_paso10, "manifiesto.yml"
  )
  archivo_recorridos <- file.path(
    directorio_recorridos_paso11, "recorridos_operativos.parquet"
  )
  manifiesto_recorridos_archivo <- file.path(
    directorio_recorridos_paso11, "manifiesto.yml"
  )
  archivo_secuencias <- file.path(
    directorio_registro_canonico, "secuencias.parquet"
  )
  manifiesto_registro_archivo <- file.path(
    directorio_registro_canonico, "manifiesto.yml"
  )
  entradas <- c(
    archivo_od_operativa, archivo_paso09, manifiesto_paso09_archivo,
    archivo_paso10, manifiesto_paso10_archivo, archivo_recorridos,
    manifiesto_recorridos_archivo, archivo_secuencias,
    manifiesto_registro_archivo
  )
  faltantes <- entradas[!file.exists(entradas)]
  if (length(faltantes)) {
    stop(sprintf(
      "Faltan archivos de entrada: %s", paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  manifiesto_paso09 <- yaml::read_yaml(manifiesto_paso09_archivo)
  manifiesto_paso10 <- yaml::read_yaml(manifiesto_paso10_archivo)
  manifiesto_recorridos <- yaml::read_yaml(manifiesto_recorridos_archivo)
  manifiesto_registro <- yaml::read_yaml(manifiesto_registro_archivo)
  fechas_entrada <- c(
    manifiesto_paso09$fecha_operativa,
    manifiesto_paso10$fecha_operativa,
    manifiesto_recorridos$fecha_operativa
  )
  if (length(fechas_entrada) != 3L || anyNA(fechas_entrada) ||
      any(as.character(fechas_entrada) != fecha)) {
    stop("Los pasos 09, 10 y 11 no corresponden a la fecha.", call. = FALSE)
  }
  metadatos_entrada <- rbind(
    c(
      manifiesto_paso09$version_registro_variantes,
      manifiesto_paso09$snapshot_referencias,
      manifiesto_paso09$red_canonica
    ),
    c(
      manifiesto_paso10$version_registro_variantes,
      manifiesto_paso10$snapshot_referencias,
      manifiesto_paso10$red_canonica
    ),
    c(
      manifiesto_recorridos$version_registro_variantes,
      manifiesto_recorridos$snapshot_referencias,
      manifiesto_recorridos$red_canonica
    ),
    c(
      manifiesto_registro$version,
      manifiesto_registro$snapshot_referencias,
      manifiesto_registro$red_canonica
    )
  )
  metadatos_coinciden <-
    !anyNA(metadatos_entrada) &&
    all(apply(metadatos_entrada, 2L, function(x) length(unique(x)) == 1L))
  if (!metadatos_coinciden) {
    stop("Las entradas no usan la misma entrega canonica.", call. = FALSE)
  }
  metadatos <- c(
    version = as.character(metadatos_entrada[1L, 1L]),
    snapshot = as.character(metadatos_entrada[1L, 2L]),
    red = as.character(metadatos_entrada[1L, 3L])
  )

  recorridos <- data.table::as.data.table(arrow::read_parquet(
    archivo_recorridos
  ))
  if (!nrow(recorridos) || anyNA(recorridos$recorrido) ||
      anyDuplicated(recorridos$recorrido)) {
    stop("El paso 11 no contiene recorridos procesables unicos.", call. = FALSE)
  }
  recorridos[, `:=`(
    recorrido = as.character(recorrido),
    id_version_variante = as.character(id_version_variante),
    tipo_servicio = as.character(tipo_servicio)
  )]
  data.table::setorder(recorridos, recorrido)

  columnas_od <- c(
    "trip_id", "cod_empresa", "numero_evento_real_subida",
    "fecha_evento_subida", "fecha_evento_bajada",
    "cantidad_pasajeros", "tipo_servicio"
  )
  od <- data.table::as.data.table(arrow::read_parquet(
    archivo_od_operativa,
    col_select = tidyselect::all_of(columnas_od)
  ))
  if (anyNA(od$trip_id) || anyDuplicated(od$trip_id)) {
    stop("La OD operativa no tiene trip_id completo y unico.", call. = FALSE)
  }
  od[, `:=`(
    cod_empresa = normalizar_clave_horario(cod_empresa),
    numero_evento_real_subida = normalizar_clave_horario(
      numero_evento_real_subida
    ),
    cantidad_pasajeros = as.numeric(cantidad_pasajeros),
    tipo_servicio = as.character(tipo_servicio),
    tiempo_subida = tiempo_local_numerico(fecha_evento_subida, zona_horaria),
    tiempo_bajada = tiempo_local_numerico(fecha_evento_bajada, zona_horaria)
  )]
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
      "punto_canonico_bajada_asignado", "recuperada"
    ))
  ))
  paso09[, `:=`(
    id_version_variante = as.character(id_version_variante),
    indice_secuencia_subida = as.integer(indice_secuencia_subida),
    indice_secuencia_bajada = as.integer(indice_secuencia_bajada),
    estado_resolucion = as.character(estado_resolucion)
  )]
  if (anyNA(paso09$trip_id) || anyDuplicated(paso09$trip_id) ||
      !setequal(paso09$trip_id, od$trip_id) ||
      anyNA(paso10$trip_id) || anyDuplicated(paso10$trip_id) ||
      !all(paso10$trip_id %in% paso09$trip_id)) {
    stop("Los pasos 09 o 10 tienen claves trip_id invalidas.", call. = FALSE)
  }
  paso10 <- paso10[, .(
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
    recuperada = as.logical(recuperada)
  )]
  base <- merge(paso09, od, by = "trip_id", all.x = TRUE, sort = FALSE)
  base <- merge(base, paso10, by = "trip_id", all.x = TRUE, sort = FALSE)
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
        apta_base, as.integer(indice_secuencia_subida), NA_integer_
      )
    ),
    indice_bajada_efectivo = data.table::fifelse(
      recuperada_paso10,
      indice_bajada_recuperada,
      data.table::fifelse(
        apta_base, as.integer(indice_secuencia_bajada), NA_integer_
      )
    )
  )]
  viajes <- base[
    viaje_elegible == TRUE & recorrido %in% recorridos$recorrido
  ]
  if (viajes[, any(
      is.na(id_version_variante_efectiva) |
        is.na(indice_subida_efectivo) |
        is.na(indice_bajada_efectivo) |
        indice_bajada_efectivo <= indice_subida_efectivo
  )]) {
    stop("Hay viajes utilizables sin posiciones canonicas validas.",
      call. = FALSE)
  }
  if (nrow(viajes) != sum(recorridos$registros_utilizables)) {
    stop("Los viajes reconstruidos no coinciden con el paso 11.", call. = FALSE)
  }
  mapa_recorridos <- recorridos[, .(
    recorrido,
    id_version_variante_catalogo = id_version_variante,
    tipo_servicio_catalogo = tipo_servicio
  )]
  viajes <- merge(
    viajes, mapa_recorridos, by = "recorrido", all.x = TRUE, sort = FALSE
  )
  if (anyNA(viajes$id_version_variante_efectiva) ||
      anyNA(viajes$id_version_variante_catalogo) ||
      anyNA(viajes$tipo_servicio) ||
      anyNA(viajes$tipo_servicio_catalogo) ||
      any(
        viajes$id_version_variante_efectiva !=
          viajes$id_version_variante_catalogo
      ) || any(viajes$tipo_servicio != viajes$tipo_servicio_catalogo)) {
    stop("Los viajes no coinciden con el catalogo de recorridos.", call. = FALSE)
  }

  secuencias <- data.table::as.data.table(arrow::read_parquet(
    archivo_secuencias,
    col_select = tidyselect::all_of(c(
      "id_version_variante", "indice_secuencia", "punto_canonico_id",
      "x_canonica", "y_canonica"
    ))
  ))
  secuencias[, `:=`(
    id_version_variante = as.character(id_version_variante),
    indice_secuencia = as.integer(indice_secuencia),
    punto_canonico_id = as.character(punto_canonico_id),
    x_canonica = as.numeric(x_canonica),
    y_canonica = as.numeric(y_canonica)
  )]
  secuencias <- secuencias[
    id_version_variante %in% recorridos$id_version_variante
  ]
  data.table::setorder(secuencias, id_version_variante, indice_secuencia)
  if (anyDuplicated(secuencias[, .(
      id_version_variante, indice_secuencia
  )]) || anyNA(secuencias[, .(
      id_version_variante, indice_secuencia, punto_canonico_id,
      x_canonica, y_canonica
  )])) {
    stop("Las secuencias canonicas tienen claves o coordenadas invalidas.", call. = FALSE)
  }
  secuencias[, distancia_tramo_m := c(
    0, sqrt(diff(x_canonica)^2 + diff(y_canonica)^2)
  ), by = id_version_variante]
  secuencias[, distancia_m := cumsum(distancia_tramo_m),
    by = id_version_variante]
  variantes_faltantes <- setdiff(
    recorridos$id_version_variante, unique(secuencias$id_version_variante)
  )
  if (length(variantes_faltantes) ||
      any(!is.finite(secuencias$distancia_m)) ||
      any(secuencias$distancia_tramo_m < 0)) {
    stop("El registro no cubre las variantes de los recorridos.", call. = FALSE)
  }

  observaciones <- data.table::rbindlist(list(
    viajes[is.finite(tiempo_subida), .(
      recorrido,
      id_version_variante = id_version_variante_efectiva,
      trip_id,
      indice_secuencia = indice_subida_efectivo,
      tiempo = tiempo_subida,
      fuente = "subida"
    )],
    viajes[
      is.finite(tiempo_bajada) & tiempo_bajada >= tiempo_subida,
      .(
        recorrido,
        id_version_variante = id_version_variante_efectiva,
        trip_id,
        indice_secuencia = indice_bajada_efectivo,
        tiempo = tiempo_bajada,
        fuente = "bajada"
      )
    ]
  ))
  cantidad_observaciones <- nrow(observaciones)
  observaciones <- merge(
    observaciones,
    secuencias[, .(
      id_version_variante, indice_secuencia, distancia_m
    )],
    by = c("id_version_variante", "indice_secuencia"),
    all = FALSE, sort = FALSE
  )
  if (nrow(observaciones) != cantidad_observaciones) {
    stop("Alguna observacion temporal no existe en la secuencia.", call. = FALSE)
  }
  observaciones_por_recorrido <- split(
    observaciones, by = "recorrido", keep.by = TRUE
  )
  anclas <- lapply(observaciones_por_recorrido, preparar_anclas_horario)
  velocidades <- unlist(
    lapply(anclas, velocidades_borde_horario), use.names = FALSE
  )
  velocidad_respaldo <- stats::median(
    velocidades[is.finite(velocidades) & velocidades > 0]
  )
  if (!is.finite(velocidad_respaldo) || velocidad_respaldo <= 0) {
    stop("No se pudo estimar una velocidad global positiva.", call. = FALSE)
  }
  secuencias_por_variante <- split(
    secuencias[, .(
      id_version_variante, indice_secuencia, punto_canonico_id, distancia_m
    )],
    by = "id_version_variante", keep.by = TRUE
  )

  dir.create(padre_salida, recursive = TRUE, showWarnings = FALSE)
  configuracion_checkpoint <- list(
    fecha_operativa = fecha,
    version_registro_variantes = unname(metadatos[["version"]]),
    snapshot_referencias = unname(metadatos[["snapshot"]]),
    red_canonica = unname(metadatos[["red"]]),
    recorridos = nrow(recorridos),
    recorridos_por_lote = recorridos_por_lote,
    velocidad_maxima_kmh = velocidad_maxima_kmh,
    ajuste_maximo_s = ajuste_maximo_s
  )
  archivo_checkpoint <- file.path(checkpoint, "checkpoint.yml")
  if (dir.exists(checkpoint)) {
    if (!file.exists(archivo_checkpoint)) {
      stop("El checkpoint de horarios esta incompleto.", call. = FALSE)
    }
    existente <- yaml::read_yaml(archivo_checkpoint)
    campos <- names(configuracion_checkpoint)
    if (!identical(
        as.character(unlist(existente[campos])),
        as.character(unlist(configuracion_checkpoint[campos]))
    )) {
      stop("El checkpoint pertenece a otra configuracion.", call. = FALSE)
    }
    directorios_checkpoint <- file.path(
      checkpoint, c("pasadas_bus", "calidad_recorridos")
    )
    if (any(!dir.exists(directorios_checkpoint))) {
      stop("El checkpoint de horarios no conserva sus particiones.",
        call. = FALSE)
    }
  } else {
    dir.create(file.path(checkpoint, "pasadas_bus"), recursive = TRUE)
    dir.create(file.path(checkpoint, "calidad_recorridos"), recursive = TRUE)
    yaml::write_yaml(configuracion_checkpoint, archivo_checkpoint)
  }

  cantidad_lotes <- ceiling(nrow(recorridos) / recorridos_por_lote)
  for (indice_lote in seq_len(cantidad_lotes)) {
    inicio <- (indice_lote - 1L) * recorridos_por_lote + 1L
    fin <- min(indice_lote * recorridos_por_lote, nrow(recorridos))
    recorridos_lote <- recorridos[inicio:fin]
    nombre_lote <- sprintf("part-%05d", indice_lote)
    archivo_pasadas <- file.path(
      checkpoint, "pasadas_bus", paste0(nombre_lote, ".parquet")
    )
    archivo_calidad <- file.path(
      checkpoint, "calidad_recorridos", paste0(nombre_lote, ".parquet")
    )
    if (file.exists(archivo_pasadas) && file.exists(archivo_calidad)) {
      message(sprintf(
        "[horarios] Lote %s/%s reutilizado.", indice_lote, cantidad_lotes
      ))
      next
    }

    tablas_lote <- vector("list", nrow(recorridos_lote))
    calidad_lote <- vector("list", nrow(recorridos_lote))
    for (indice in seq_len(nrow(recorridos_lote))) {
      recorrido_actual <- recorridos_lote$recorrido[[indice]]
      id_variante <- recorridos_lote$id_version_variante[[indice]]
      secuencia <- secuencias_por_variante[[id_variante]]
      ancla <- anclas[[recorrido_actual]]
      if (is.null(ancla)) ancla <- data.table::data.table()
      ajuste <- estimar_tiempos_secuencia(
        secuencia$distancia_m, ancla, velocidad_respaldo
      )
      velocidades_borde <- if (nrow(ancla)) {
        velocidades_borde_horario(ancla)
      } else {
        c(inicio = NA_real_, final = NA_real_)
      }
      calidad_actual <- data.table::data.table(
        recorrido = recorrido_actual,
        id_version_variante = id_variante,
        tipo_servicio = recorridos_lote$tipo_servicio[[indice]],
        registros_utilizables = recorridos_lote$registros_utilizables[[indice]],
        personas_utilizables = recorridos_lote$personas_utilizables[[indice]],
        posiciones = nrow(secuencia),
        anclas = nrow(ancla),
        posiciones_sin_hora = sum(!is.finite(ajuste$tiempo)),
        anclas_ajustadas = if (nrow(ancla)) {
          sum(abs(ancla$ajuste_s) > 1e-6)
        } else {
          0L
        },
        ajuste_max_s = if (nrow(ancla)) {
          max(abs(ancla$ajuste_s))
        } else {
          NA_real_
        },
        velocidad_inicio_kmh = data.table::fifelse(
          is.na(velocidades_borde[[1L]]),
          velocidad_respaldo,
          velocidades_borde[[1L]]
        ) * 3.6,
        velocidad_final_kmh = data.table::fifelse(
          is.na(velocidades_borde[[2L]]),
          velocidad_respaldo,
          velocidades_borde[[2L]]
        ) * 3.6,
        respaldo_global = anyNA(velocidades_borde)
      )
      calidad_actual <- clasificar_calidad_horario(
        calidad_actual, velocidad_maxima_kmh, ajuste_maximo_s
      )
      calidad_lote[[indice]] <- calidad_actual
      if (calidad_actual$estado_horario == "horario_completo") {
        tablas_lote[[indice]] <- data.table::data.table(
          recorrido = recorrido_actual,
          id_version_variante = id_variante,
          tipo_servicio = recorridos_lote$tipo_servicio[[indice]],
          indice_secuencia = secuencia$indice_secuencia,
          punto_canonico_id = secuencia$punto_canonico_id,
          distancia_m = secuencia$distancia_m,
          tiempo = ajuste$tiempo,
          metodo_tiempo = ajuste$metodo_tiempo,
          buses = 1L
        )
      }
    }
    pasadas_lote <- data.table::rbindlist(
      tablas_lote, use.names = TRUE, fill = TRUE
    )
    if (!nrow(pasadas_lote)) pasadas_lote <- tabla_pasadas_vacia()
    calidad_lote <- data.table::rbindlist(calidad_lote, use.names = TRUE)
    temporal_pasadas <- paste0(archivo_pasadas, ".tmp")
    temporal_calidad <- paste0(archivo_calidad, ".tmp")
    if (!file.exists(archivo_pasadas)) {
      arrow::write_parquet(
        pasadas_lote, temporal_pasadas, compression = "zstd"
      )
      if (!file.rename(temporal_pasadas, archivo_pasadas)) {
        stop("No se pudo promover un lote de pasadas.", call. = FALSE)
      }
    }
    if (!file.exists(archivo_calidad)) {
      arrow::write_parquet(
        calidad_lote, temporal_calidad, compression = "zstd"
      )
      if (!file.rename(temporal_calidad, archivo_calidad)) {
        stop("No se pudo promover un lote de calidad.", call. = FALSE)
      }
    }
    message(sprintf(
      "[horarios] Lote %s/%s terminado.", indice_lote, cantidad_lotes
    ))
  }

  archivos_calidad <- list.files(
    file.path(checkpoint, "calidad_recorridos"),
    pattern = "[.]parquet$", full.names = TRUE
  )
  archivos_pasadas <- list.files(
    file.path(checkpoint, "pasadas_bus"),
    pattern = "[.]parquet$", full.names = TRUE
  )
  if (length(archivos_calidad) != cantidad_lotes ||
      length(archivos_pasadas) != cantidad_lotes) {
    stop("No se completaron todos los lotes de horarios.", call. = FALSE)
  }
  calidad <- data.table::rbindlist(lapply(
    archivos_calidad, function(x) data.table::as.data.table(
      arrow::read_parquet(x)
    )
  ))
  if (nrow(calidad) != nrow(recorridos) ||
      anyDuplicated(calidad$recorrido)) {
    stop("La calidad no conserva un registro por recorrido.", call. = FALSE)
  }

  filas_pasadas <- 0L
  duplicados_pasadas <- FALSE
  faltantes_pasadas <- FALSE
  recorridos_pasadas <- character()
  metodos <- list()
  for (indice in seq_along(archivos_pasadas)) {
    parte <- data.table::as.data.table(arrow::read_parquet(
      archivos_pasadas[[indice]],
      col_select = tidyselect::all_of(c(
        "recorrido", "indice_secuencia", "punto_canonico_id",
        "tiempo", "metodo_tiempo", "tipo_servicio"
      ))
    ))
    filas_pasadas <- filas_pasadas + nrow(parte)
    duplicados_pasadas <- duplicados_pasadas || anyDuplicated(
      parte[, .(recorrido, indice_secuencia)]
    ) > 0L
    faltantes_pasadas <- faltantes_pasadas ||
      anyNA(parte[, .(
        recorrido, indice_secuencia, punto_canonico_id,
        metodo_tiempo, tipo_servicio
      )]) || any(!is.finite(parte$tiempo))
    recorridos_pasadas <- c(
      recorridos_pasadas, unique(parte$recorrido)
    )
    metodos[[indice]] <- parte[, .N,
      by = .(tipo_servicio, metodo_tiempo)]
  }
  recorridos_pasadas <- unique(recorridos_pasadas)
  metodos <- data.table::rbindlist(metodos)[, .(
    posiciones = sum(N)
  ), by = .(tipo_servicio, metodo_tiempo)]
  metodos_total <- metodos[, .(
    tipo_servicio = "total",
    posiciones = sum(posiciones)
  ), by = metodo_tiempo]
  metodos <- data.table::rbindlist(
    list(metodos, metodos_total), use.names = TRUE
  )[order(tipo_servicio, metodo_tiempo)]

  aprobados <- calidad[estado_horario == "horario_completo"]
  cuarentena <- calidad[estado_horario != "horario_completo"]
  resumen <- calidad[, .(
    recorridos = .N,
    registros_utilizables = sum(registros_utilizables),
    personas_utilizables = sum(personas_utilizables),
    posiciones_secuencia = sum(posiciones),
    posiciones_publicadas = if (
      identical(estado_horario[[1L]], "horario_completo")
    ) sum(posiciones) else 0L
  ), by = .(tipo_servicio, estado_horario)]
  resumen_total <- calidad[, .(
    tipo_servicio = "total",
    recorridos = .N,
    registros_utilizables = sum(registros_utilizables),
    personas_utilizables = sum(personas_utilizables),
    posiciones_secuencia = sum(posiciones),
    posiciones_publicadas = if (
      identical(estado_horario[[1L]], "horario_completo")
    ) sum(posiciones) else 0L
  ), by = estado_horario]
  resumen <- data.table::rbindlist(
    list(resumen, resumen_total), use.names = TRUE
  )[order(tipo_servicio, estado_horario)]

  controles <- data.table::data.table(
    control = c(
      "entregas_canonicas_coinciden", "recorridos_paso11_unicos",
      "viajes_reconstruidos_coinciden", "recuperaciones_incorporadas",
      "viajes_coinciden_con_recorrido", "secuencias_cubren_variantes",
      "observaciones_en_secuencia", "velocidad_respaldo_positiva",
      "calidad_unica_por_recorrido", "pasadas_sin_duplicados",
      "pasadas_completas", "filas_pasadas_coinciden",
      "cuarentena_fuera_de_pasadas"
    ),
    aprobado = c(
      metadatos_coinciden,
      anyDuplicated(recorridos$recorrido) == 0L,
      nrow(viajes) == sum(recorridos$registros_utilizables),
      viajes[recuperada_paso10 == TRUE, .N] ==
        sum(recorridos$registros_recuperados),
      all(
        viajes$id_version_variante_efectiva ==
          viajes$id_version_variante_catalogo
      ),
      !length(variantes_faltantes),
      nrow(observaciones) == cantidad_observaciones,
      is.finite(velocidad_respaldo) && velocidad_respaldo > 0,
      nrow(calidad) == nrow(recorridos) &&
        anyDuplicated(calidad$recorrido) == 0L,
      !duplicados_pasadas,
      !faltantes_pasadas,
      filas_pasadas == sum(aprobados$posiciones),
      !any(cuarentena$recorrido %in% recorridos_pasadas)
    )
  )
  controles[, estado := data.table::fifelse(aprobado, "aprobado", "fallo")]
  if (any(!controles$aprobado)) {
    stop("Los horarios no aprobaron sus controles.", call. = FALSE)
  }

  data.table::fwrite(
    cuarentena, file.path(checkpoint, "recorridos_cuarentena_temporal.csv")
  )
  data.table::fwrite(
    resumen, file.path(checkpoint, "resumen_horarios.csv")
  )
  data.table::fwrite(
    metodos, file.path(checkpoint, "metodos_resumen.csv")
  )
  data.table::fwrite(controles, file.path(checkpoint, "controles.csv"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    tipo_salida = "horarios_recorridos_particionados",
    fuente_recorridos = normalizePath(
      directorio_recorridos_paso11, winslash = "/", mustWork = TRUE
    ),
    registro_canonico = normalizePath(
      directorio_registro_canonico, winslash = "/", mustWork = TRUE
    ),
    version_registro_variantes = metadatos[["version"]],
    snapshot_referencias = metadatos[["snapshot"]],
    red_canonica = metadatos[["red"]],
    zona_horaria = zona_horaria,
    recorridos_por_lote = recorridos_por_lote,
    cantidad_lotes = cantidad_lotes,
    velocidad_respaldo_kmh = velocidad_respaldo * 3.6,
    velocidad_maxima_kmh = velocidad_maxima_kmh,
    ajuste_maximo_s = ajuste_maximo_s,
    recorridos_evaluados = nrow(calidad),
    recorridos_aprobados = nrow(aprobados),
    recorridos_cuarentena = nrow(cuarentena),
    filas_pasadas = filas_pasadas,
    incluye_tabla_viajes = FALSE,
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(checkpoint, "manifiesto.yml"))
  if (!file.rename(checkpoint, destino)) {
    stop(sprintf("No se pudo promover la salida de horarios: %s", destino),
      call. = FALSE)
  }
  list(
    destino = destino,
    calidad = calidad,
    cuarentena = cuarentena,
    resumen = resumen,
    controles = controles,
    velocidad_respaldo_kmh = velocidad_respaldo * 3.6
  )
}
