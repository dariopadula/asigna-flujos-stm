normalizar_clave_recorrido <- function(x) {
  valor <- trimws(as.character(x))
  valor[is.na(x) | !nzchar(valor) | valor == "NA"] <- NA_character_
  valor
}

valor_unico_o_na <- function(x) {
  valores <- unique(stats::na.omit(as.character(x)))
  if (length(valores) == 1L) valores[[1L]] else NA_character_
}

construir_recorridos_operativos <- function(
    fecha,
    archivo_od_operativa,
    directorio_resolucion_paso09,
    directorio_recuperacion_paso10,
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
    directorio_salida, "recorridos_operativos", paste0("fecha=", fecha)
  )
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf(
      "Los recorridos operativos ya existen y no seran sobrescritos: %s",
      destino
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
  entradas <- c(
    archivo_od_operativa, archivo_paso09, manifiesto_paso09_archivo,
    archivo_paso10, manifiesto_paso10_archivo
  )
  faltantes <- entradas[!file.exists(entradas)]
  if (length(faltantes)) {
    stop(sprintf(
      "Faltan archivos de entrada: %s", paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  manifiesto_paso09 <- yaml::read_yaml(manifiesto_paso09_archivo)
  manifiesto_paso10 <- yaml::read_yaml(manifiesto_paso10_archivo)
  if (!identical(as.character(manifiesto_paso09$fecha_operativa), fecha) ||
      !identical(as.character(manifiesto_paso10$fecha_operativa), fecha)) {
    stop("Los pasos 09 y 10 no corresponden a la fecha solicitada.", call. = FALSE)
  }
  metadatos_paso09 <- c(
    version = manifiesto_paso09$version_registro_variantes,
    snapshot = manifiesto_paso09$snapshot_referencias,
    red = manifiesto_paso09$red_canonica
  )
  metadatos_paso10 <- c(
    version = manifiesto_paso10$version_registro_variantes,
    snapshot = manifiesto_paso10$snapshot_referencias,
    red = manifiesto_paso10$red_canonica
  )
  metadatos_coinciden <-
    !anyNA(metadatos_paso09) && !anyNA(metadatos_paso10) &&
    identical(as.character(metadatos_paso09), as.character(metadatos_paso10))
  if (!metadatos_coinciden) {
    stop("Los pasos 09 y 10 no usan la misma entrega canonica.", call. = FALSE)
  }

  columnas_od <- c(
    "trip_id", "cod_empresa", "numero_evento_real_subida",
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
    cod_empresa = normalizar_clave_recorrido(cod_empresa),
    numero_evento_real_subida = normalizar_clave_recorrido(
      numero_evento_real_subida
    ),
    cantidad_pasajeros = as.numeric(cantidad_pasajeros),
    tipo_servicio = as.character(tipo_servicio)
  )]
  od[, clave_recorrido_valida :=
    !is.na(cod_empresa) & !is.na(numero_evento_real_subida)]
  od[, recorrido := data.table::fifelse(
    clave_recorrido_valida,
    paste(fecha, cod_empresa, numero_evento_real_subida, sep = "|"),
    NA_character_
  )]

  columnas_paso09 <- c(
    "trip_id", "id_version_variante", "indice_secuencia_subida",
    "indice_secuencia_bajada", "punto_canonico_subida",
    "punto_canonico_bajada", "estado_resolucion"
  )
  paso09 <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso09,
    col_select = tidyselect::all_of(columnas_paso09)
  ))
  if (anyNA(paso09$trip_id) || anyDuplicated(paso09$trip_id) ||
      !setequal(paso09$trip_id, od$trip_id)) {
    stop("El paso 09 no conserva exactamente el universo de la OD.", call. = FALSE)
  }
  paso09[, `:=`(
    id_version_variante = as.character(id_version_variante),
    indice_secuencia_subida = as.integer(indice_secuencia_subida),
    indice_secuencia_bajada = as.integer(indice_secuencia_bajada),
    punto_canonico_subida = as.character(punto_canonico_subida),
    punto_canonico_bajada = as.character(punto_canonico_bajada),
    estado_resolucion = as.character(estado_resolucion)
  )]

  columnas_paso10 <- c(
    "trip_id", "id_version_variante_subida", "indice_secuencia_subida",
    "punto_canonico_subida", "indice_secuencia_bajada_asignado",
    "punto_canonico_bajada_asignado", "recuperada", "requiere_revision",
    "accion_resolucion"
  )
  paso10 <- data.table::as.data.table(arrow::read_parquet(
    archivo_paso10,
    col_select = tidyselect::all_of(columnas_paso10)
  ))
  if (anyNA(paso10$trip_id) || anyDuplicated(paso10$trip_id) ||
      !all(paso10$trip_id %in% paso09$trip_id)) {
    stop("El delta del paso 10 tiene una clave trip_id invalida.", call. = FALSE)
  }
  paso10[, `:=`(
    id_version_variante_subida = as.character(id_version_variante_subida),
    indice_secuencia_subida = as.integer(indice_secuencia_subida),
    punto_canonico_subida = as.character(punto_canonico_subida),
    indice_secuencia_bajada_asignado = as.integer(
      indice_secuencia_bajada_asignado
    ),
    punto_canonico_bajada_asignado = as.character(
      punto_canonico_bajada_asignado
    ),
    recuperada = as.logical(recuperada),
    requiere_revision = as.logical(requiere_revision),
    accion_resolucion = as.character(accion_resolucion)
  )]

  base <- merge(paso09, od, by = "trip_id", all.x = TRUE, sort = FALSE)
  delta <- paso10[, .(
    trip_id,
    id_version_variante_recuperada = id_version_variante_subida,
    indice_subida_recuperada = indice_secuencia_subida,
    punto_subida_recuperado = punto_canonico_subida,
    indice_bajada_recuperada = indice_secuencia_bajada_asignado,
    punto_bajada_recuperado = punto_canonico_bajada_asignado,
    recuperada,
    requiere_revision,
    accion_recuperacion = accion_resolucion
  )]
  base <- merge(base, delta, by = "trip_id", all.x = TRUE, sort = FALSE)
  if (nrow(base) != nrow(od) || anyDuplicated(base$trip_id)) {
    stop("La integracion de los pasos 09 y 10 altero el universo OD.", call. = FALSE)
  }
  if (!all(paso10$trip_id %in% base[
      estado_resolucion == "variante_subida_bajada_distinta", trip_id
  ])) {
    stop("El paso 10 contiene viajes fuera de su cuarentena objetivo.", call. = FALSE)
  }

  base[, apta_base := estado_resolucion == "apta_secuencia"]
  base[, recuperada_paso10 :=
    estado_resolucion == "variante_subida_bajada_distinta" &
      !is.na(recuperada) & recuperada == TRUE]
  versiones_recuperadas_coinciden <- base[recuperada_paso10 == TRUE, all(
    !is.na(id_version_variante_recuperada) &
      !is.na(id_version_variante) &
      id_version_variante_recuperada == id_version_variante
  )]
  if (!versiones_recuperadas_coinciden) {
    stop(
      "La version recuperada no coincide con la variante de subida del paso 09.",
      call. = FALSE
    )
  }
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
      data.table::fifelse(apta_base, indice_secuencia_subida, NA_integer_)
    ),
    indice_bajada_efectivo = data.table::fifelse(
      recuperada_paso10,
      indice_bajada_recuperada,
      data.table::fifelse(apta_base, indice_secuencia_bajada, NA_integer_)
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

  if (base[viaje_elegible == TRUE, any(
      is.na(id_version_variante_efectiva) |
        is.na(indice_subida_efectivo) | is.na(indice_bajada_efectivo) |
        is.na(punto_subida_efectivo) | is.na(punto_bajada_efectivo) |
        indice_bajada_efectivo <= indice_subida_efectivo
  )]) {
    stop("Hay viajes elegibles sin posiciones canonicas validas.", call. = FALSE)
  }

  auditoria <- base[, {
    variantes <- unique(stats::na.omit(id_version_variante))
    tipos <- unique(stats::na.omit(tipo_servicio))
    .(
      cod_empresa = valor_unico_o_na(cod_empresa),
      numero_evento_real_subida = valor_unico_o_na(
        numero_evento_real_subida
      ),
      registros_total = .N,
      personas_total = sum(cantidad_pasajeros, na.rm = TRUE),
      variantes = length(variantes),
      id_version_variante = if (length(variantes) == 1L) {
        variantes[[1L]]
      } else {
        NA_character_
      },
      tipos_servicio = length(tipos),
      tipo_servicio = if (length(tipos) == 1L) tipos[[1L]] else NA_character_,
      clave_valida = all(clave_recorrido_valida),
      registros_aptos_base = sum(apta_base),
      registros_recuperados = sum(recuperada_paso10),
      personas_elegibles = sum(
        cantidad_pasajeros[viaje_elegible], na.rm = TRUE
      ),
      recuperados_revision = sum(
        recuperada_paso10 & requiere_revision == TRUE, na.rm = TRUE
      ),
      indice_observado_min = if (any(viaje_elegible)) {
        min(indice_subida_efectivo[viaje_elegible], na.rm = TRUE)
      } else {
        NA_integer_
      },
      indice_observado_max = if (any(viaje_elegible)) {
        max(indice_bajada_efectivo[viaje_elegible], na.rm = TRUE)
      } else {
        NA_integer_
      }
    )
  }, by = recorrido]
  auditoria[, registros_elegibles :=
    registros_aptos_base + registros_recuperados]
  auditoria[, estado_recorrido := data.table::fcase(
    !clave_valida, "clave_recorrido_invalida",
    variantes > 1L, "variantes_ambiguas",
    tipos_servicio != 1L, "tipos_servicio_ambiguos",
    is.na(id_version_variante), "sin_variante_canonica",
    registros_elegibles == 0L, "sin_viajes_elegibles",
    default = "procesable"
  )]
  auditoria[, registros_utilizables := data.table::fifelse(
    estado_recorrido == "procesable", registros_elegibles, 0L
  )]
  auditoria[, personas_utilizables := data.table::fifelse(
    estado_recorrido == "procesable", personas_elegibles, 0
  )]

  recorridos <- auditoria[estado_recorrido == "procesable", .(
    fecha_operativa = fecha,
    recorrido,
    cod_empresa,
    numero_evento_real_subida,
    id_version_variante,
    tipo_servicio,
    registros_total,
    personas_total,
    registros_aptos_base,
    registros_recuperados,
    registros_utilizables,
    personas_utilizables,
    recuperados_revision,
    indice_observado_min,
    indice_observado_max,
    version_registro_variantes = as.character(metadatos_paso09[["version"]]),
    snapshot_referencias = as.character(metadatos_paso09[["snapshot"]]),
    red_canonica = as.character(metadatos_paso09[["red"]])
  )]
  data.table::setorder(recorridos, recorrido)
  cuarentena <- auditoria[estado_recorrido != "procesable"]
  data.table::setorder(cuarentena, estado_recorrido, recorrido)

  resumen <- auditoria[, .(
    recorridos = .N,
    registros_total = sum(registros_total),
    personas_total = sum(personas_total),
    registros_elegibles = sum(registros_elegibles),
    personas_elegibles = sum(personas_elegibles),
    registros_utilizables = sum(registros_utilizables),
    personas_utilizables = sum(personas_utilizables),
    registros_recuperados = sum(registros_recuperados)
  ), by = .(
    tipo_servicio = data.table::fifelse(
      is.na(tipo_servicio), "mixto_o_desconocido", tipo_servicio
    ),
    estado_recorrido
  )]
  resumen_total <- auditoria[, .(
    tipo_servicio = "total",
    recorridos = .N,
    registros_total = sum(registros_total),
    personas_total = sum(personas_total),
    registros_elegibles = sum(registros_elegibles),
    personas_elegibles = sum(personas_elegibles),
    registros_utilizables = sum(registros_utilizables),
    personas_utilizables = sum(personas_utilizables),
    registros_recuperados = sum(registros_recuperados)
  ), by = estado_recorrido]
  resumen <- data.table::rbindlist(
    list(resumen, resumen_total), use.names = TRUE
  )[order(tipo_servicio, estado_recorrido)]

  recorridos_procesables <- recorridos$recorrido
  viajes_utilizables <- base[
    viaje_elegible == TRUE & recorrido %in% recorridos_procesables
  ]
  controles <- data.table::data.table(
    control = c(
      "universo_paso09_conservado", "delta_paso10_valido",
      "entregas_canonicas_coinciden", "viajes_elegibles_completos",
      "viajes_elegibles_orden_positivo", "recuperaciones_misma_version",
      "recuperaciones_incorporadas",
      "recorrido_procesable_unico", "recorrido_una_variante",
      "recorrido_un_tipo_servicio", "viajes_utilizables_en_recorrido",
      "registros_auditados_conservados", "personas_auditadas_conservadas"
    ),
    aprobado = c(
      nrow(base) == nrow(od) && setequal(base$trip_id, od$trip_id),
      all(paso10$trip_id %in% base[
        estado_resolucion == "variante_subida_bajada_distinta", trip_id
      ]),
      metadatos_coinciden,
      base[viaje_elegible == TRUE, all(
        !is.na(id_version_variante_efectiva) &
          !is.na(indice_subida_efectivo) &
          !is.na(indice_bajada_efectivo) &
          !is.na(punto_subida_efectivo) &
          !is.na(punto_bajada_efectivo)
      )],
      base[viaje_elegible == TRUE, all(
        indice_bajada_efectivo > indice_subida_efectivo
      )],
      versiones_recuperadas_coinciden,
      base[recuperada_paso10 == TRUE, .N] ==
        paso10[recuperada == TRUE, .N],
      anyDuplicated(recorridos$recorrido) == 0L,
      auditoria[estado_recorrido == "procesable", all(variantes == 1L)],
      auditoria[
        estado_recorrido == "procesable", all(tipos_servicio == 1L)
      ],
      nrow(viajes_utilizables) == sum(recorridos$registros_utilizables),
      sum(auditoria$registros_total) == nrow(od),
      isTRUE(all.equal(
        sum(auditoria$personas_total),
        sum(od$cantidad_pasajeros, na.rm = TRUE)
      ))
    )
  )
  controles[, estado := data.table::fifelse(aprobado, "aprobado", "fallo")]
  if (any(!controles$aprobado)) {
    stop("Los recorridos operativos no aprobaron sus controles.", call. = FALSE)
  }

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
    recorridos,
    file.path(temporal, "recorridos_operativos.parquet"),
    compression = "zstd"
  )
  data.table::fwrite(
    cuarentena, file.path(temporal, "recorridos_cuarentena.csv")
  )
  data.table::fwrite(resumen, file.path(temporal, "resumen_recorridos.csv"))
  data.table::fwrite(controles, file.path(temporal, "controles.csv"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    tipo_salida = "catalogo_recorridos_operativos",
    clave_recorrido =
      "fecha_operativa|cod_empresa|numero_evento_real_subida",
    fuente_paso09 = normalizePath(
      directorio_resolucion_paso09, winslash = "/", mustWork = TRUE
    ),
    fuente_paso10 = normalizePath(
      directorio_recuperacion_paso10, winslash = "/", mustWork = TRUE
    ),
    version_registro_variantes = metadatos_paso09[["version"]],
    snapshot_referencias = metadatos_paso09[["snapshot"]],
    red_canonica = metadatos_paso09[["red"]],
    recorridos_auditados = nrow(auditoria),
    recorridos_procesables = nrow(recorridos),
    recorridos_cuarentena = nrow(cuarentena),
    registros_utilizables = sum(recorridos$registros_utilizables),
    personas_utilizables = sum(recorridos$personas_utilizables),
    registros_recuperados_utilizables = sum(recorridos$registros_recuperados),
    incluye_tabla_viajes = FALSE,
    incluye_geometrias = FALSE,
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))
  if (!file.rename(temporal, destino)) {
    stop(sprintf(
      "No se pudo promover el catalogo de recorridos: %s", destino
    ), call. = FALSE)
  }
  terminado <- TRUE
  list(
    destino = destino,
    recorridos = recorridos,
    cuarentena = cuarentena,
    resumen = resumen,
    controles = controles
  )
}
