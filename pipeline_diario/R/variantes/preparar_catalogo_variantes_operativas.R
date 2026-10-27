preparar_catalogo_variantes_operativas <- function(
    fecha,
    archivo_diagnostico,
    archivo_geometrias,
    archivo_paradas_normalizadas,
    archivo_resumen_paradas,
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
  entradas <- c(
    archivo_diagnostico, archivo_geometrias,
    archivo_paradas_normalizadas, archivo_resumen_paradas
  )
  inexistentes <- entradas[!file.exists(entradas)]
  if (length(inexistentes)) {
    stop(sprintf("Faltan archivos de entrada: %s", paste(inexistentes, collapse = ", ")), call. = FALSE)
  }

  destino <- file.path(directorio_salida, "catalogo_variantes_operativas", paste0("fecha=", fecha))
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf("El catalogo ya existe y no sera sobrescrito: %s", destino), call. = FALSE)
  }

  diagnostico <- data.table::fread(archivo_diagnostico, showProgress = FALSE)
  columnas_diagnostico <- c("codigo_variante", "geometria_estado", "fecha_vigencia")
  faltantes_diagnostico <- setdiff(columnas_diagnostico, names(diagnostico))
  if (length(faltantes_diagnostico)) {
    stop(sprintf(
      "Faltan columnas en el diagnostico: %s",
      paste(faltantes_diagnostico, collapse = ", ")
    ), call. = FALSE)
  }
  diagnostico[, codigo_variante := as.character(codigo_variante)]
  if (anyDuplicated(diagnostico$codigo_variante)) {
    stop("El diagnostico contiene mas de una fila por variante.", call. = FALSE)
  }

  resumen_paradas <- data.table::fread(archivo_resumen_paradas, showProgress = FALSE)
  resumen_paradas[, codigo_variante := as.character(codigo_variante)]
  if (anyDuplicated(resumen_paradas$codigo_variante)) {
    stop("El resumen de paradas contiene mas de una fila por variante.", call. = FALSE)
  }

  paradas <- data.table::as.data.table(arrow::read_parquet(archivo_paradas_normalizadas))
  if (!"codigo_variante" %in% names(paradas)) {
    stop("Las paradas normalizadas no contienen codigo_variante.", call. = FALSE)
  }
  paradas[, codigo_variante := as.character(codigo_variante)]

  columnas_geometria <- c(
    "codigo", "sentido", "fecha_vigencia", "fecha_baja",
    "nombre_variante", "the_geom"
  )
  geometrias <- data.table::fread(
    archivo_geometrias,
    select = columnas_geometria,
    showProgress = FALSE,
    encoding = "UTF-8"
  )
  geometrias[, `:=`(
    fila_fuente_geometria = seq_len(.N),
    codigo_variante = as.character(codigo),
    vigencia_date = as.Date(substr(fecha_vigencia, 1L, 10L)),
    baja_date = as.Date(substr(fecha_baja, 1L, 10L))
  )]
  geometrias <- geometrias[
    codigo_variante %in% diagnostico$codigo_variante &
      !is.na(vigencia_date) & vigencia_date <= fecha_date
  ]
  data.table::setorder(geometrias, codigo_variante, vigencia_date, fila_fuente_geometria)
  geometrias <- geometrias[, .SD[.N], by = codigo_variante]
  geometrias <- geometrias[, .(
    codigo_variante,
    sentido_fuente = sentido,
    nombre_variante_fuente = nombre_variante,
    fecha_vigencia_fuente = as.character(vigencia_date),
    fecha_baja_fuente = as.character(baja_date),
    geometria_wkt = the_geom
  )]

  catalogo <- merge(diagnostico, resumen_paradas, by = "codigo_variante", all.x = TRUE)
  catalogo <- merge(catalogo, geometrias, by = "codigo_variante", all.x = TRUE)
  catalogo[, tiene_paradas_normalizadas := !is.na(filas_normalizadas) & filas_normalizadas > 0L]
  catalogo[, requiere_resolucion_espacial :=
    !is.na(ordinales_positivos_compartidos) & ordinales_positivos_compartidos > 0L]
  catalogo[, estado_catalogo := data.table::fcase(
    geometria_estado != "vigente_valida", "pendiente_geometria",
    is.na(geometria_wkt) | !nzchar(trimws(geometria_wkt)), "pendiente_geometria",
    !tiene_paradas_normalizadas, "pendiente_paradas",
    !is.na(filas_sin_coordenadas) & filas_sin_coordenadas > 0L, "pendiente_coordenadas",
    default = "lista_para_proyeccion"
  )]
  catalogo[, lista_para_proyeccion := estado_catalogo == "lista_para_proyeccion"]
  catalogo[, id_version_variante := data.table::fifelse(
    lista_para_proyeccion,
    paste0(codigo_variante, "_", gsub("-", "", fecha_vigencia_fuente)),
    NA_character_
  )]
  data.table::setorder(catalogo, estado_catalogo, codigo_variante)

  codigos_listos <- catalogo[lista_para_proyeccion == TRUE, .(
    codigo_variante, id_version_variante
  )]
  paradas_catalogo <- merge(
    paradas, codigos_listos,
    by = "codigo_variante", all = FALSE
  )
  data.table::setcolorder(
    paradas_catalogo,
    c("id_version_variante", "codigo_variante", setdiff(names(paradas_catalogo), c(
      "id_version_variante", "codigo_variante"
    )))
  )
  data.table::setorder(
    paradas_catalogo, id_version_variante, ordinal_ancla,
    cod_ubic_parada, na.last = TRUE
  )

  pendientes <- catalogo[lista_para_proyeccion == FALSE, .(
    codigo_variante,
    estado_catalogo,
    geometria_estado,
    fecha_vigencia,
    filas_normalizadas,
    registros_subida,
    registros_bajada
  )]
  resumen <- catalogo[, .(
    variantes = .N,
    filas_paradas = sum(filas_normalizadas, na.rm = TRUE),
    registros_subida = sum(registros_subida, na.rm = TRUE),
    registros_bajada = sum(registros_bajada, na.rm = TRUE)
  ), by = estado_catalogo][order(estado_catalogo)]
  resumen <- data.table::rbindlist(list(resumen, data.table::data.table(
    estado_catalogo = "TOTAL",
    variantes = nrow(catalogo),
    filas_paradas = sum(catalogo$filas_normalizadas, na.rm = TRUE),
    registros_subida = sum(catalogo$registros_subida, na.rm = TRUE),
    registros_bajada = sum(catalogo$registros_bajada, na.rm = TRUE)
  )), use.names = TRUE)

  if (nrow(catalogo) != data.table::uniqueN(diagnostico$codigo_variante)) {
    stop("El catalogo no conserva exactamente el universo diagnosticado.", call. = FALSE)
  }
  if (anyDuplicated(catalogo$id_version_variante[!is.na(catalogo$id_version_variante)])) {
    stop("La clave de version no es unica.", call. = FALSE)
  }
  if (paradas_catalogo[, data.table::uniqueN(codigo_variante)] != nrow(codigos_listos)) {
    stop("Alguna variante lista quedo sin paradas en el catalogo.", call. = FALSE)
  }

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(dirname(destino), sprintf(".tmp_fecha_%s_%s", gsub("-", "", fecha), Sys.getpid()))
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) unlink(temporal, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  arrow::write_parquet(catalogo, file.path(temporal, "catalogo_variantes.parquet"), compression = "zstd")
  arrow::write_parquet(
    paradas_catalogo,
    file.path(temporal, "paradas_variantes_catalogo.parquet"),
    compression = "zstd"
  )
  data.table::fwrite(pendientes, file.path(temporal, "variantes_pendientes.csv"))
  data.table::fwrite(resumen, file.path(temporal, "resumen_catalogo.csv"))
  yaml::write_yaml(list(
    fecha_objetivo = fecha,
    criterio_version = "codigo_variante + fecha_vigencia de la ultima version al corte",
    variantes_catalogadas = nrow(catalogo),
    variantes_listas = catalogo[lista_para_proyeccion == TRUE, .N],
    variantes_pendientes = catalogo[lista_para_proyeccion == FALSE, .N],
    filas_paradas_listas = nrow(paradas_catalogo),
    politica_pendientes = "se registran; no se infiere ni reemplaza su geometria",
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))

  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover la salida temporal a: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  list(destino = destino, resumen = resumen, catalogo = catalogo, pendientes = pendientes)
}
