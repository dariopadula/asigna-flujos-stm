normalizar_clave_parada <- function(x) {
  valor <- trimws(as.character(x))
  valor[is.na(x) | !nzchar(valor) | valor == "NA"] <- NA_character_
  valor
}

resolver_paradas_canonicas_dia <- function(
    fecha,
    archivo_od_operativa,
    directorio_uso_variantes,
    directorio_registro_canonico,
    directorio_salida) {
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

  destino <- file.path(
    directorio_salida, "od_paradas_canonicas", paste0("fecha=", fecha)
  )
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf(
      "La resolucion diaria de paradas ya existe y no sera sobrescrita: %s",
      destino
    ), call. = FALSE)
  }

  archivo_uso <- file.path(directorio_uso_variantes, "uso_variantes.parquet")
  manifiesto_uso_archivo <- file.path(directorio_uso_variantes, "manifiesto.yml")
  archivo_paradas <- file.path(
    directorio_registro_canonico, "paradas_proyectadas.parquet"
  )
  manifiesto_registro_archivo <- file.path(
    directorio_registro_canonico, "manifiesto.yml"
  )
  entradas <- c(
    archivo_od_operativa, archivo_uso, manifiesto_uso_archivo,
    archivo_paradas, manifiesto_registro_archivo
  )
  faltantes <- entradas[!file.exists(entradas)]
  if (length(faltantes)) {
    stop(sprintf(
      "Faltan archivos de entrada: %s", paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  manifiesto_uso <- yaml::read_yaml(manifiesto_uso_archivo)
  manifiesto_registro <- yaml::read_yaml(manifiesto_registro_archivo)
  if (!identical(as.character(manifiesto_uso$fecha_operativa), fecha)) {
    stop("El uso de variantes no corresponde a la fecha solicitada.", call. = FALSE)
  }
  if (!identical(manifiesto_registro$etapa, "registro_canonico") ||
      !identical(manifiesto_registro$estado, "completo")) {
    stop("El registro canonico no esta promovido o esta incompleto.", call. = FALSE)
  }

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
  if (anyNA(metadatos_uso) || anyNA(metadatos_registro) ||
      !identical(as.character(metadatos_uso), as.character(metadatos_registro))) {
    stop(
      "El uso diario y el registro canonico no identifican la misma entrega.",
      call. = FALSE
    )
  }

  columnas_od <- c(
    "trip_id", "sevar_codigo_subida", "sevar_codigo_bajada",
    "codigo_parada_subida", "codigo_parada_bajada",
    "cantidad_pasajeros", "tipo_servicio"
  )
  esquema_od <- names(arrow::open_dataset(
    archivo_od_operativa, format = "parquet"
  )$schema)
  faltantes_od <- setdiff(columnas_od, esquema_od)
  if (length(faltantes_od)) {
    stop(sprintf(
      "Faltan columnas en la OD operativa: %s",
      paste(faltantes_od, collapse = ", ")
    ), call. = FALSE)
  }
  od <- data.table::as.data.table(arrow::read_parquet(
    archivo_od_operativa,
    col_select = tidyselect::all_of(columnas_od)
  ))
  if (!nrow(od)) stop("La OD operativa esta vacia.", call. = FALSE)
  if (anyNA(od$trip_id) || anyDuplicated(od$trip_id)) {
    stop("trip_id debe ser completo y unico en la OD operativa.", call. = FALSE)
  }
  od[, `:=`(
    codigo_variante_subida = normalizar_clave_parada(sevar_codigo_subida),
    codigo_variante_bajada = normalizar_clave_parada(sevar_codigo_bajada),
    codigo_parada_subida = normalizar_clave_parada(codigo_parada_subida),
    codigo_parada_bajada = normalizar_clave_parada(codigo_parada_bajada),
    cantidad_pasajeros = as.numeric(cantidad_pasajeros),
    tipo_servicio = as.character(tipo_servicio)
  )]

  columnas_uso <- c(
    "codigo_variante", "id_version_variante", "estado_resolucion"
  )
  uso <- data.table::as.data.table(arrow::read_parquet(
    archivo_uso,
    col_select = tidyselect::all_of(columnas_uso)
  ))
  uso[, `:=`(
    codigo_variante = normalizar_clave_parada(codigo_variante),
    id_version_variante = as.character(id_version_variante),
    estado_variante = as.character(estado_resolucion)
  )]
  uso[, estado_resolucion := NULL]
  if (anyDuplicated(uso$codigo_variante)) {
    stop("El uso diario contiene mas de una fila por variante.", call. = FALSE)
  }
  codigos_requeridos <- unique(stats::na.omit(od$codigo_variante_subida))
  codigos_sin_uso <- setdiff(codigos_requeridos, uso$codigo_variante)
  if (length(codigos_sin_uso)) {
    stop(sprintf(
      "Hay variantes de subida ausentes del paso 08: %s",
      paste(codigos_sin_uso, collapse = ", ")
    ), call. = FALSE)
  }

  uso_subida <- uso[, .(
    codigo_variante_subida = codigo_variante,
    id_version_variante,
    estado_variante
  )]
  base <- merge(
    od[, .(
      trip_id, codigo_variante_subida, codigo_variante_bajada,
      codigo_parada_subida, codigo_parada_bajada,
      cantidad_pasajeros, tipo_servicio
    )],
    uso_subida,
    by = "codigo_variante_subida", all.x = TRUE, sort = FALSE
  )
  if (nrow(base) != nrow(od) || anyDuplicated(base$trip_id)) {
    stop("La union con el uso de variantes altero las filas de la OD.", call. = FALSE)
  }

  columnas_paradas <- c(
    "id_version_variante", "cod_ubic_parada", "indice_secuencia",
    "punto_canonico_id", "estado_calidad", "metodo_asignacion",
    "distancia_asignada_m"
  )
  esquema_paradas <- names(arrow::open_dataset(
    archivo_paradas, format = "parquet"
  )$schema)
  faltantes_paradas <- setdiff(columnas_paradas, esquema_paradas)
  if (length(faltantes_paradas)) {
    stop(sprintf(
      "Faltan columnas en las paradas canonicas: %s",
      paste(faltantes_paradas, collapse = ", ")
    ), call. = FALSE)
  }
  paradas <- data.table::as.data.table(arrow::read_parquet(
    archivo_paradas,
    col_select = tidyselect::all_of(columnas_paradas)
  ))
  paradas[, `:=`(
    id_version_variante = as.character(id_version_variante),
    codigo_parada = normalizar_clave_parada(cod_ubic_parada),
    indice_secuencia = as.integer(indice_secuencia),
    punto_canonico_id = as.character(punto_canonico_id),
    estado_calidad = as.character(estado_calidad),
    metodo_asignacion = as.character(metodo_asignacion),
    distancia_asignada_m = as.numeric(distancia_asignada_m)
  )]
  paradas[, cod_ubic_parada := NULL]
  paradas <- unique(paradas)
  clave_parada <- c("id_version_variante", "codigo_parada", "indice_secuencia")
  if (anyDuplicated(paradas, by = clave_parada)) {
    stop("Las paradas canonicas tienen una clave de posicion ambigua.", call. = FALSE)
  }
  if (anyNA(paradas[, ..clave_parada]) || anyNA(paradas$punto_canonico_id)) {
    stop("Las paradas canonicas contienen claves incompletas.", call. = FALSE)
  }

  resultado <- base[, .(
    fecha_operativa = fecha,
    trip_id,
    codigo_variante_subida,
    codigo_variante_bajada,
    id_version_variante,
    codigo_parada_subida,
    codigo_parada_bajada,
    indice_secuencia_subida = NA_integer_,
    indice_secuencia_bajada = NA_integer_,
    punto_canonico_subida = NA_character_,
    punto_canonico_bajada = NA_character_,
    estado_calidad_subida = NA_character_,
    estado_calidad_bajada = NA_character_,
    metodo_asignacion_subida = NA_character_,
    metodo_asignacion_bajada = NA_character_,
    distancia_asignada_subida_m = NA_real_,
    distancia_asignada_bajada_m = NA_real_,
    tramo_posiciones = NA_integer_,
    combinaciones_candidatas = 0L,
    estado_resolucion = data.table::fcase(
      is.na(codigo_variante_subida) | is.na(codigo_variante_bajada) |
        codigo_variante_subida != codigo_variante_bajada,
      "variante_subida_bajada_distinta",
      is.na(id_version_variante) | estado_variante != "resuelta",
      "sin_variante_canonica",
      default = "pendiente_resolucion"
    )
  )]

  elegibles <- base[
    !is.na(codigo_variante_subida) &
      codigo_variante_subida == codigo_variante_bajada &
      estado_variante == "resuelta" & !is.na(id_version_variante)
  ]
  if (nrow(elegibles)) {
    origen <- paradas[, .(
      id_version_variante,
      codigo_parada_subida = codigo_parada,
      indice_secuencia_subida = indice_secuencia,
      punto_canonico_subida = punto_canonico_id,
      estado_calidad_subida = estado_calidad,
      metodo_asignacion_subida = metodo_asignacion,
      distancia_asignada_subida_m = distancia_asignada_m
    )]
    bajada <- paradas[, .(
      id_version_variante,
      codigo_parada_bajada = codigo_parada,
      indice_secuencia_bajada = indice_secuencia,
      punto_canonico_bajada = punto_canonico_id,
      estado_calidad_bajada = estado_calidad,
      metodo_asignacion_bajada = metodo_asignacion,
      distancia_asignada_bajada_m = distancia_asignada_m
    )]
    pares <- merge(
      elegibles[, .(
        trip_id, id_version_variante,
        codigo_parada_subida, codigo_parada_bajada
      )],
      origen,
      by = c("id_version_variante", "codigo_parada_subida"),
      all.x = TRUE, allow.cartesian = TRUE, sort = FALSE
    )
    pares <- merge(
      pares,
      bajada,
      by = c("id_version_variante", "codigo_parada_bajada"),
      all.x = TRUE, allow.cartesian = TRUE, sort = FALSE
    )
    pares[, tramo_posiciones := indice_secuencia_bajada - indice_secuencia_subida]
    pares[, orden_resolucion := data.table::fcase(
      is.finite(tramo_posiciones) & tramo_posiciones > 0L, 1L,
      is.finite(tramo_posiciones) & tramo_posiciones == 0L, 2L,
      is.finite(tramo_posiciones), 3L,
      default = 4L
    )]
    pares[, tramo_absoluto := abs(tramo_posiciones)]
    data.table::setorder(
      pares, trip_id, orden_resolucion, tramo_absoluto,
      indice_secuencia_subida, indice_secuencia_bajada, na.last = TRUE
    )
    conteos <- pares[, .(combinaciones_candidatas = .N), by = trip_id]
    resueltos <- pares[, .SD[1L], by = trip_id]
    resueltos <- conteos[resueltos, on = "trip_id"]
    resueltos[, estado_resolucion := data.table::fcase(
      is.na(indice_secuencia_subida) & is.na(indice_secuencia_bajada),
      "sin_ambas_paradas",
      is.na(indice_secuencia_subida), "sin_parada_subida",
      is.na(indice_secuencia_bajada), "sin_parada_bajada",
      tramo_posiciones > 0L, "apta_secuencia",
      tramo_posiciones == 0L, "misma_posicion",
      default = "orden_inverso"
    )]
    resultado[resueltos, on = "trip_id", `:=`(
      indice_secuencia_subida = i.indice_secuencia_subida,
      indice_secuencia_bajada = i.indice_secuencia_bajada,
      punto_canonico_subida = i.punto_canonico_subida,
      punto_canonico_bajada = i.punto_canonico_bajada,
      estado_calidad_subida = i.estado_calidad_subida,
      estado_calidad_bajada = i.estado_calidad_bajada,
      metodo_asignacion_subida = i.metodo_asignacion_subida,
      metodo_asignacion_bajada = i.metodo_asignacion_bajada,
      distancia_asignada_subida_m = i.distancia_asignada_subida_m,
      distancia_asignada_bajada_m = i.distancia_asignada_bajada_m,
      tramo_posiciones = i.tramo_posiciones,
      combinaciones_candidatas = i.combinaciones_candidatas,
      estado_resolucion = i.estado_resolucion
    )]
  }

  resultado[, `:=`(
    version_registro_variantes = as.character(metadatos_registro[["version"]]),
    snapshot_referencias = as.character(metadatos_registro[["snapshot"]]),
    red_canonica = as.character(metadatos_registro[["red"]])
  )]
  data.table::setorder(resultado, trip_id)

  personas_od <- sum(base$cantidad_pasajeros, na.rm = TRUE)
  estados_validos <- c(
    "apta_secuencia", "misma_posicion", "orden_inverso",
    "sin_parada_subida", "sin_parada_bajada", "sin_ambas_paradas",
    "sin_variante_canonica", "variante_subida_bajada_distinta"
  )
  controles <- data.table::data.table(
    control = c(
      "trip_id_diario_unico", "universo_od_conservado",
      "uso_variantes_cubre_subidas", "entrega_canonica_coincide",
      "estados_resolucion_validos", "aptas_con_posiciones_completas",
      "aptas_con_orden_positivo", "variantes_distintas_en_cuarentena",
      "registros_conservados", "personas_conservadas"
    ),
    aprobado = c(
      anyDuplicated(resultado$trip_id) == 0L,
      setequal(resultado$trip_id, od$trip_id),
      !length(codigos_sin_uso),
      identical(as.character(metadatos_uso), as.character(metadatos_registro)),
      all(resultado$estado_resolucion %in% estados_validos),
      resultado[estado_resolucion == "apta_secuencia", all(
        !is.na(indice_secuencia_subida) & !is.na(indice_secuencia_bajada) &
          !is.na(punto_canonico_subida) & !is.na(punto_canonico_bajada)
      )],
      resultado[estado_resolucion == "apta_secuencia", all(tramo_posiciones > 0L)],
      resultado[
        is.na(codigo_variante_subida) | is.na(codigo_variante_bajada) |
          codigo_variante_subida != codigo_variante_bajada,
        all(estado_resolucion == "variante_subida_bajada_distinta")
      ],
      nrow(resultado) == nrow(od),
      isTRUE(all.equal(
        personas_od,
        sum(base$cantidad_pasajeros[match(resultado$trip_id, base$trip_id)], na.rm = TRUE)
      ))
    )
  )
  controles[, estado := data.table::fifelse(aprobado, "aprobado", "fallo")]
  if (any(!controles$aprobado)) {
    stop("La resolucion diaria de paradas no aprobo sus controles.", call. = FALSE)
  }

  auditoria <- merge(
    resultado[, .(
      trip_id, codigo_variante_subida, codigo_variante_bajada,
      estado_resolucion
    )],
    base[, .(trip_id, tipo_servicio, cantidad_pasajeros)],
    by = "trip_id", all.x = TRUE, sort = FALSE
  )
  resumen <- auditoria[, .(
    registros = .N,
    personas = sum(cantidad_pasajeros, na.rm = TRUE),
    variantes_subida = data.table::uniqueN(
      codigo_variante_subida, na.rm = TRUE
    )
  ), by = .(tipo_servicio, estado_resolucion)]
  resumen_total <- auditoria[, .(
    tipo_servicio = "total",
    registros = .N,
    personas = sum(cantidad_pasajeros, na.rm = TRUE),
    variantes_subida = data.table::uniqueN(
      codigo_variante_subida, na.rm = TRUE
    )
  ), by = estado_resolucion]
  resumen <- data.table::rbindlist(
    list(resumen, resumen_total), use.names = TRUE
  )[order(tipo_servicio, estado_resolucion)]
  pendientes <- auditoria[estado_resolucion != "apta_secuencia", .(
    registros = .N,
    personas = sum(cantidad_pasajeros, na.rm = TRUE)
  ), by = .(
    tipo_servicio, estado_resolucion,
    codigo_variante_subida, codigo_variante_bajada
  )][order(tipo_servicio, estado_resolucion, -registros)]

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
    file.path(temporal, "resolucion_paradas.parquet"),
    compression = "zstd"
  )
  data.table::fwrite(resumen, file.path(temporal, "resumen_resolucion.csv"))
  data.table::fwrite(
    pendientes, file.path(temporal, "pendientes_por_variante.csv")
  )
  data.table::fwrite(controles, file.path(temporal, "controles.csv"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    tipo_salida = "relacion_diaria_paradas_canonicas",
    fuente_od = normalizePath(
      archivo_od_operativa, winslash = "/", mustWork = TRUE
    ),
    uso_variantes = normalizePath(
      directorio_uso_variantes, winslash = "/", mustWork = TRUE
    ),
    registro_canonico = normalizePath(
      directorio_registro_canonico, winslash = "/", mustWork = TRUE
    ),
    version_registro_variantes = metadatos_registro[["version"]],
    snapshot_referencias = metadatos_registro[["snapshot"]],
    red_canonica = metadatos_registro[["red"]],
    politica_variante = "usar_variante_subida_y_cuarentenar_si_difiere_bajada",
    politica_repeticiones = "tramo_positivo_mas_corto",
    registros = nrow(resultado),
    personas = personas_od,
    registros_aptos = resultado[estado_resolucion == "apta_secuencia", .N],
    registros_pendientes = resultado[estado_resolucion != "apta_secuencia", .N],
    incluye_geometrias = FALSE,
    incluye_secuencias_completas = FALSE,
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))

  if (!file.rename(temporal, destino)) {
    stop(sprintf(
      "No se pudo promover la resolucion diaria de paradas: %s", destino
    ), call. = FALSE)
  }
  terminado <- TRUE
  list(
    destino = destino,
    resolucion = resultado,
    resumen = resumen,
    pendientes = pendientes,
    controles = controles
  )
}
