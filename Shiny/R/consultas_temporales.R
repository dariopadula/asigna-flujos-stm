operational_date_from_run <- function(run_id) {
  value <- substr(as.character(run_id), 1L, 10L)
  valid <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)
  result <- as.Date(rep(NA_character_, length(value)))
  result[valid] <- as.Date(value[valid])
  result
}

read_daily_catalog <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("No existe el catalogo diario: %s.", path), call. = FALSE)
  }
  catalog <- data.table::fread(path, colClasses = list(character = c(
    "fecha_operativa", "estado", "directorio", "archivo_bus",
    "archivo_paradas", "archivo_viajes", "archivo_manifiesto"
  )))
  required <- c(
    "fecha_operativa", "estado", "directorio", "archivo_bus",
    "archivo_paradas", "archivo_viajes", "archivo_manifiesto"
  )
  missing <- setdiff(required, names(catalog))
  if (length(missing)) {
    stop(sprintf("El catalogo diario no contiene: %s.", paste(missing, collapse = ", ")), call. = FALSE)
  }
  parsed <- as.Date(catalog$fecha_operativa)
  if (anyNA(parsed)) stop("El catalogo diario contiene fechas invalidas.", call. = FALSE)
  if (anyDuplicated(parsed)) stop("El catalogo diario duplica fechas operativas.", call. = FALSE)
  catalog[, fecha_operativa := parsed]
  catalog[order(fecha_operativa)]
}

eligible_daily_catalog <- function(catalog) {
  catalog[estado == "listo"]
}

resolve_daily_file <- function(catalog_path, row, column) {
  stopifnot(nrow(row) == 1L, column %in% names(row))
  base <- dirname(normalizePath(catalog_path, winslash = "/", mustWork = TRUE))
  path <- file.path(base, row$directorio[[1]], row[[column]][[1]])
  if (!file.exists(path)) {
    stop(sprintf("Falta el artefacto diario '%s' para %s.", column, row$fecha_operativa), call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

load_daily_tables <- function(catalog_path, catalog, date) {
  selected_date <- as.Date(date)
  row <- catalog[fecha_operativa == selected_date & estado == "listo"]
  if (nrow(row) != 1L) {
    stop(sprintf("La fecha operativa %s no esta publicada.", selected_date), call. = FALSE)
  }
  list(
    bus = data.table::as.data.table(arrow::read_parquet(
      resolve_daily_file(catalog_path, row, "archivo_bus")
    )),
    stop_passes = data.table::as.data.table(arrow::read_parquet(
      resolve_daily_file(catalog_path, row, "archivo_paradas")
    )),
    trips = data.table::as.data.table(arrow::read_parquet(
      resolve_daily_file(catalog_path, row, "archivo_viajes")
    ))
  )
}

seconds_of_local_day <- function(value, timezone = "America/Montevideo") {
  instant <- as.POSIXct(value, origin = "1970-01-01", tz = timezone)
  parts <- as.POSIXlt(instant, tz = timezone)
  parts$hour * 3600 + parts$min * 60 + floor(parts$sec)
}

filter_operational_window <- function(x, date, hours, time_column) {
  stopifnot(length(hours) == 2L, hours[[2]] > hours[[1]], time_column %in% names(x))
  if ("fecha_operativa" %in% names(x)) {
    selected_date <- as.Date(date)
    seconds <- seconds_of_local_day(x[[time_column]])
    return(x[
      as.Date(fecha_operativa) == selected_date &
        seconds >= hours[[1]] * 3600 & seconds < hours[[2]] * 3600
    ])
  }
  start <- as.numeric(as.POSIXct(as.character(date), tz = "America/Montevideo"))
  x[get(time_column) >= start + hours[[1]] * 3600 &
      get(time_column) < start + hours[[2]] * 3600]
}

