preparar_od_operativa <- function(
    fecha,
    directorio_od_diaria,
    archivo_catalogo_lineas,
    directorio_salida,
    columnas_operativas) {
  for (paquete in c("arrow", "data.table", "dplyr", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }

  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", fecha) || is.na(as.Date(fecha))) {
    stop("La fecha debe tener formato AAAA-MM-DD.", call. = FALSE)
  }

  particion_entrada <- file.path(directorio_od_diaria, paste0("fecha=", fecha))
  archivos_od <- list.files(particion_entrada, pattern = "[.]parquet$", full.names = TRUE)
  if (!length(archivos_od)) {
    stop(sprintf("No existe la ingesta diaria para %s.", fecha), call. = FALSE)
  }
  if (!file.exists(archivo_catalogo_lineas)) {
    stop(sprintf("No existe el catalogo de lineas: %s", archivo_catalogo_lineas), call. = FALSE)
  }

  destino <- file.path(directorio_salida, "od_operativa", paste0("fecha=", fecha))
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf("La OD operativa ya existe y no sera sobrescrita: %s", destino), call. = FALSE)
  }

  dataset <- arrow::open_dataset(archivos_od, format = "parquet")
  faltantes <- setdiff(columnas_operativas, names(dataset$schema))
  if (length(faltantes)) {
    stop(
      sprintf("Faltan columnas en la ingesta diaria: %s", paste(faltantes, collapse = ", ")),
      call. = FALSE
    )
  }

  od <- dataset |>
    dplyr::filter(.data$global %in% c("True", "TRUE", "true", "1", "T")) |>
    dplyr::select(dplyr::all_of(columnas_operativas)) |>
    dplyr::collect() |>
    data.table::as.data.table()
  if (!nrow(od)) {
    stop(sprintf("No hay registros globales para %s.", fecha), call. = FALSE)
  }

  catalogo <- data.table::fread(
    archivo_catalogo_lineas,
    showProgress = FALSE,
    encoding = "UTF-8"
  )
  data.table::setnames(catalogo, sub("^lineas_metro[.]", "", names(catalogo)))
  columnas_catalogo <- c("anio_mes", "codigo", "codigo_subsistema", "nombre_publico", "cod_empresa")
  faltantes_catalogo <- setdiff(columnas_catalogo, names(catalogo))
  if (length(faltantes_catalogo)) {
    stop(
      sprintf("Faltan columnas en lineas_metro.csv: %s", paste(faltantes_catalogo, collapse = ", ")),
      call. = FALSE
    )
  }

  anio_mes_objetivo <- as.integer(format(as.Date(fecha), "%Y%m"))
  catalogo <- unique(catalogo[catalogo$anio_mes == anio_mes_objetivo, .(
    linea_catalogo_key = as.character(codigo),
    codigo_subsistema,
    nombre_publico,
    empresa_catalogo = cod_empresa
  )])
  if (anyDuplicated(catalogo$linea_catalogo_key)) {
    duplicadas <- unique(catalogo[duplicated(linea_catalogo_key), linea_catalogo_key])
    stop(
      sprintf("El catalogo mensual tiene lineas duplicadas: %s", paste(duplicadas, collapse = ", ")),
      call. = FALSE
    )
  }

  od[, linea_catalogo_key := as.character(linea_codigo_subida)]
  lineas_sin_catalogo <- sort(setdiff(unique(od$linea_catalogo_key), catalogo$linea_catalogo_key))
  if (length(lineas_sin_catalogo)) {
    stop(
      sprintf("Lineas OD sin catalogo mensual: %s", paste(lineas_sin_catalogo, collapse = ", ")),
      call. = FALSE
    )
  }

  od <- catalogo[od, on = "linea_catalogo_key"]
  od[, linea_catalogo_key := NULL]

  texto_fecha <- function(x) {
    if (inherits(x, "POSIXt")) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC") else as.character(x)
  }
  od[, `:=`(
    fecha_evento_subida = texto_fecha(fecha_evento_subida),
    fecha_evento_bajada = texto_fecha(fecha_evento_bajada),
    numero_evento_real_subida = as.character(numero_evento_real_subida),
    numero_evento_real_bajada = as.character(numero_evento_real_bajada),
    id_recorrido_global = as.character(id_recorrido_global),
    tipo_servicio = data.table::fifelse(codigo_subsistema == 1L, "urbano", "inter"),
    fecha_operativa = as.Date(fecha),
    global = TRUE,
    trip_id = seq_len(.N)
  )]

  resumen <- od[, .(
    registros = .N,
    personas = sum(as.numeric(cantidad_pasajeros), na.rm = TRUE),
    lineas = data.table::uniqueN(linea_codigo_subida),
    variantes = data.table::uniqueN(sevar_codigo_subida),
    recorridos_reales = data.table::uniqueN(numero_evento_real_subida),
    variantes_subida_bajada_distintas = sum(
      as.character(sevar_codigo_subida) != as.character(sevar_codigo_bajada),
      na.rm = TRUE
    )
  ), by = .(codigo_subsistema, tipo_servicio)][order(codigo_subsistema)]
  resumen_total <- od[, .(
    codigo_subsistema = NA_integer_,
    tipo_servicio = "total",
    registros = .N,
    personas = sum(as.numeric(cantidad_pasajeros), na.rm = TRUE),
    lineas = data.table::uniqueN(linea_codigo_subida),
    variantes = data.table::uniqueN(sevar_codigo_subida),
    recorridos_reales = data.table::uniqueN(numero_evento_real_subida),
    variantes_subida_bajada_distintas = sum(
      as.character(sevar_codigo_subida) != as.character(sevar_codigo_bajada),
      na.rm = TRUE
    )
  )]
  resumen <- data.table::rbindlist(list(resumen, resumen_total), use.names = TRUE)

  padre_destino <- dirname(destino)
  dir.create(padre_destino, recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(padre_destino, sprintf(".tmp_fecha_%s_%s", gsub("-", "", fecha), Sys.getpid()))
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) unlink(temporal, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  salida_parquet <- file.path(temporal, "od_operativa.parquet")
  arrow::write_parquet(od, salida_parquet, compression = "zstd")
  data.table::fwrite(resumen, file.path(temporal, "resumen_od_operativa.csv"))
  data.table::fwrite(
    data.table::data.table(linea_codigo_subida = character()),
    file.path(temporal, "lineas_sin_catalogo.csv")
  )
  yaml::write_yaml(list(
    fecha_objetivo = fecha,
    fuente_od = normalizePath(particion_entrada, winslash = "/", mustWork = TRUE),
    catalogo_lineas = normalizePath(archivo_catalogo_lineas, winslash = "/", mustWork = TRUE),
    filas = nrow(od),
    personas = sum(as.numeric(od$cantidad_pasajeros), na.rm = TRUE),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))

  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover la salida temporal a: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  list(destino = destino, resumen = resumen)
}

