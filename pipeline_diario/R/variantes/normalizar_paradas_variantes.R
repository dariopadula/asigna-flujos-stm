normalizar_paradas_variantes <- function(
    fecha,
    archivo_diagnostico,
    archivos_paradas,
    directorio_salida) {
  for (paquete in c("arrow", "data.table", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  fecha_date <- as.Date(fecha)
  if (length(fecha) != 1L || is.na(fecha_date) || format(fecha_date, "%Y-%m-%d") != fecha) {
    stop("La fecha debe tener formato AAAA-MM-DD.", call. = FALSE)
  }
  entradas <- c(archivo_diagnostico, archivos_paradas)
  inexistentes <- entradas[!file.exists(entradas)]
  if (length(inexistentes)) {
    stop(sprintf("Faltan archivos de entrada: %s", paste(inexistentes, collapse = ", ")), call. = FALSE)
  }

  destino <- file.path(directorio_salida, "paradas_variantes_normalizadas", paste0("fecha=", fecha))
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf("La normalizacion ya existe y no sera sobrescrita: %s", destino), call. = FALSE)
  }

  diagnostico <- data.table::fread(archivo_diagnostico, select = "codigo_variante")
  codigos <- unique(as.character(diagnostico$codigo_variante))
  anio_mes_objetivo <- as.integer(format(fecha_date, "%Y%m"))
  columnas <- c(
    "cod_ubic_parada", "desc_ubic_parada", "cod_nombre_via_1",
    "cod_nombre_via_2", "nom_calle", "cod_variante", "ordinal",
    "cod_linea", "anio_mes", "x", "y"
  )
  partes <- lapply(seq_along(archivos_paradas), function(indice) {
    datos <- data.table::fread(
      archivos_paradas[[indice]], select = columnas,
      showProgress = FALSE, encoding = "UTF-8"
    )
    datos <- datos[anio_mes == anio_mes_objetivo]
    datos[, `:=`(
      particion_fuente = basename(archivos_paradas[[indice]]),
      fila_particion = seq_len(.N)
    )]
    datos
  })
  fuente <- data.table::rbindlist(partes, fill = TRUE)
  fuente[, codigo_variante := as.character(cod_variante)]
  fuente <- fuente[codigo_variante %in% codigos]
  fuente[, fila_union := seq_len(.N)]

  clave_exacta <- c("codigo_variante", "cod_ubic_parada", "ordinal", "x", "y")
  auditoria <- fuente[, .(
    fila_elegida = min(fila_union),
    repeticiones_fuente = .N,
    particiones_origen = paste(sort(unique(particion_fuente)), collapse = "|")
  ), by = clave_exacta]
  normalizadas <- fuente[auditoria$fila_elegida]
  normalizadas[, `:=`(
    repeticiones_fuente = auditoria$repeticiones_fuente,
    particiones_origen = auditoria$particiones_origen,
    ordinal_original = as.integer(ordinal),
    ordinal_ancla = data.table::fifelse(!is.na(ordinal) & ordinal > 0L, as.integer(ordinal), NA_integer_),
    coordenadas_validas = is.finite(x) & is.finite(y)
  )]

  ordinales_compartidos <- normalizadas[!is.na(ordinal_ancla), .N,
    by = .(codigo_variante, ordinal_ancla)][N > 1L]
  clave_compartida <- paste(
    ordinales_compartidos$codigo_variante,
    ordinales_compartidos$ordinal_ancla,
    sep = "|"
  )
  normalizadas[, ordinal_positivo_compartido := paste(
    codigo_variante, ordinal_ancla, sep = "|"
  ) %in% clave_compartida & !is.na(ordinal_ancla)]
  normalizadas[, criterio_orden_preliminar := data.table::fcase(
    !coordenadas_validas, "sin_coordenadas",
    ordinal_positivo_compartido, "ordinal_compartido_proyectar",
    is.na(ordinal_ancla), "solo_proyeccion_espacial",
    default = "ordinal_ancla_y_proyeccion"
  )]

  resumen_variantes <- normalizadas[, .(
    filas_normalizadas = .N,
    paradas_distintas = data.table::uniqueN(cod_ubic_parada, na.rm = TRUE),
    duplicados_exactos_eliminados = sum(repeticiones_fuente - 1L),
    filas_sin_ordinal_ancla = sum(is.na(ordinal_ancla)),
    ordinales_positivos_compartidos = data.table::uniqueN(
      ordinal_ancla[ordinal_positivo_compartido], na.rm = TRUE
    ),
    filas_sin_coordenadas = sum(!coordenadas_validas)
  ), by = codigo_variante]
  resumen_variantes[, estado_normalizacion := data.table::fcase(
    filas_sin_coordenadas > 0L, "revisar_coordenadas",
    ordinales_positivos_compartidos > 0L, "lista_con_ordinal_compartido",
    default = "lista_para_proyeccion"
  )]
  data.table::setorder(normalizadas, codigo_variante, ordinal_ancla, cod_ubic_parada, na.last = TRUE)
  data.table::setorder(resumen_variantes, estado_normalizacion, codigo_variante)

  resumen <- resumen_variantes[, .(
    variantes = .N,
    filas_normalizadas = sum(filas_normalizadas),
    duplicados_exactos_eliminados = sum(duplicados_exactos_eliminados),
    filas_sin_ordinal_ancla = sum(filas_sin_ordinal_ancla),
    ordinales_positivos_compartidos = sum(ordinales_positivos_compartidos),
    filas_sin_coordenadas = sum(filas_sin_coordenadas)
  ), by = estado_normalizacion][order(estado_normalizacion)]
  resumen <- data.table::rbindlist(list(resumen, data.table::data.table(
    estado_normalizacion = "TOTAL",
    variantes = nrow(resumen_variantes),
    filas_normalizadas = nrow(normalizadas),
    duplicados_exactos_eliminados = sum(normalizadas$repeticiones_fuente - 1L),
    filas_sin_ordinal_ancla = sum(is.na(normalizadas$ordinal_ancla)),
    ordinales_positivos_compartidos = nrow(ordinales_compartidos),
    filas_sin_coordenadas = sum(!normalizadas$coordenadas_validas)
  )), use.names = TRUE)

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(dirname(destino), sprintf(".tmp_fecha_%s_%s", gsub("-", "", fecha), Sys.getpid()))
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) unlink(temporal, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  arrow::write_parquet(normalizadas, file.path(temporal, "paradas_normalizadas.parquet"), compression = "zstd")
  data.table::fwrite(resumen_variantes, file.path(temporal, "resumen_por_variante.csv"))
  data.table::fwrite(
    normalizadas[ordinal_positivo_compartido | !coordenadas_validas],
    file.path(temporal, "paradas_revision.csv")
  )
  data.table::fwrite(resumen, file.path(temporal, "resumen_normalizacion.csv"))
  yaml::write_yaml(list(
    fecha_objetivo = fecha,
    anio_mes = anio_mes_objetivo,
    criterio = list(
      duplicado_exacto = "codigo_variante + parada + ordinal + x + y",
      ordinal_ancla = "ordinal > 0",
      saltos = "se conservan; no implican paradas faltantes",
      orden_final = "se definira por proyeccion espacial sobre la geometria"
    ),
    filas_fuente = nrow(fuente),
    filas_normalizadas = nrow(normalizadas),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))

  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover la salida temporal a: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  list(destino = destino, resumen = resumen, resumen_variantes = resumen_variantes)
}

