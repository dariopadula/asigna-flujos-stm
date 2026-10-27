validar_fecha_iso <- function(fecha) {
  if (length(fecha) != 1L || is.na(fecha) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", fecha)) {
    stop("La fecha debe tener formato AAAA-MM-DD.", call. = FALSE)
  }

  fecha_date <- as.Date(fecha)
  if (is.na(fecha_date) || format(fecha_date, "%Y-%m-%d") != fecha) {
    stop(sprintf("La fecha no es valida: %s", fecha), call. = FALSE)
  }

  fecha_date
}

verificar_columnas <- function(columnas_disponibles, columnas_requeridas) {
  faltantes <- setdiff(columnas_requeridas, columnas_disponibles)
  if (length(faltantes)) {
    stop(
      sprintf("Faltan columnas requeridas en la OD: %s", paste(faltantes, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

abrir_od_csv <- function(archivo_od) {
  fuente_inferida <- arrow::open_dataset(archivo_od, format = "csv")
  esquema <- fuente_inferida$schema
  columnas_texto <- intersect(
    c(
      "numero_tarjeta_mifare", "numero_evento_real_subida",
      "numero_evento_real_bajada", "desc_linea_subida", "desc_linea_bajada",
      "nom_calle_subida", "nom_calle_bajada", "variante_info_subida",
      "sentido_variante_subida", "variante_info_bajada",
      "sentido_variante_bajada", "id_recorrido_global", "valid",
      "extrapolation", "global"
    ),
    names(esquema)
  )

  for (columna in columnas_texto) {
    indice <- match(columna, names(esquema)) - 1L
    esquema <- esquema$SetField(indice, arrow::field(columna, arrow::utf8()))
  }

  # Al proporcionar un esquema explícito, Arrow necesita que se omita la fila
  # de encabezados del CSV.
  arrow::open_dataset(archivo_od, format = "csv", schema = esquema, skip = 1L)
}

resumir_od_ingerida <- function(dataset, columnas_clave) {
  columnas_control <- unique(c(
    "fecha_evento_subida", "linea_codigo_subida", "sevar_codigo_subida",
    "codigo_parada_subida", "codigo_parada_bajada", "cantidad_pasajeros",
    "global", "valid", columnas_clave
  ))
  datos <- data.table::as.data.table(dplyr::collect(dplyr::select(
    dataset,
    dplyr::all_of(columnas_control)
  )))

  es_verdadero <- function(x) {
    toupper(trimws(as.character(x))) %in% c("TRUE", "1", "T")
  }
  registros_globales <- es_verdadero(datos$global)
  registros_validos <- es_verdadero(datos$valid)

  resumen <- data.frame(
    filas = nrow(datos),
    filas_global = sum(registros_globales, na.rm = TRUE),
    filas_valid = sum(registros_validos, na.rm = TRUE),
    personas = sum(as.numeric(datos$cantidad_pasajeros), na.rm = TRUE),
    personas_global = sum(
      as.numeric(datos$cantidad_pasajeros[registros_globales]),
      na.rm = TRUE
    ),
    # El Parquet conserva el timestamp sin zona de la fuente. UTC evita que R
    # desplace visualmente la hora local al construir el resumen.
    fecha_min = format(min(datos$fecha_evento_subida), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    fecha_max = format(max(datos$fecha_evento_subida), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    lineas_subida = data.table::uniqueN(datos$linea_codigo_subida, na.rm = TRUE),
    variantes_subida = data.table::uniqueN(datos$sevar_codigo_subida, na.rm = TRUE),
    paradas_subida = data.table::uniqueN(datos$codigo_parada_subida, na.rm = TRUE),
    paradas_bajada = data.table::uniqueN(datos$codigo_parada_bajada, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  faltantes <- lapply(columnas_clave, function(columna) {
    cantidad_faltantes <- sum(is.na(datos[[columna]]))
    data.frame(
      columna = columna,
      filas = nrow(datos),
      faltantes = cantidad_faltantes,
      porcentaje_faltante = if (nrow(datos) == 0) {
        NA_real_
      } else {
        100 * cantidad_faltantes / nrow(datos)
      },
      stringsAsFactors = FALSE
    )
  })

  list(
    resumen = resumen,
    faltantes = data.table::rbindlist(faltantes)
  )
}

ingerir_od_dia <- function(
    fecha,
    archivo_od,
    directorio_salida,
    columnas_requeridas,
    columnas_clave,
    zona_horaria = "America/Montevideo") {
  for (paquete in c("arrow", "data.table", "dplyr", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }

  fecha_date <- validar_fecha_iso(fecha)
  if (!file.exists(archivo_od)) {
    stop(sprintf("No existe el archivo OD: %s", archivo_od), call. = FALSE)
  }

  destino <- file.path(directorio_salida, "od", paste0("fecha=", fecha))
  if (dir.exists(destino) || file.exists(destino)) {
    stop(
      sprintf("La ingesta ya existe y no sera sobrescrita: %s", destino),
      call. = FALSE
    )
  }

  fuente <- abrir_od_csv(archivo_od)
  verificar_columnas(names(fuente$schema), columnas_requeridas)

  # Arrow interpreta la columna como timestamp sin zona. UTC conserva literalmente
  # las horas del CSV; la zona de Montevideo se registra como metadato operativo.
  inicio <- as.POSIXct(fecha, tz = "UTC")
  fin <- as.POSIXct(fecha_date + 1L, tz = "UTC")
  seleccion <- dplyr::filter(
    fuente,
    .data$fecha_evento_subida >= inicio,
    .data$fecha_evento_subida < fin
  )

  padre_destino <- dirname(destino)
  dir.create(padre_destino, recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(
    padre_destino,
    sprintf(".tmp_fecha_%s_%s", gsub("-", "", fecha), Sys.getpid())
  )
  if (dir.exists(temporal)) {
    unlink(temporal, recursive = TRUE, force = TRUE)
  }
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)

  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) {
      unlink(temporal, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  arrow::write_dataset(
    seleccion,
    path = temporal,
    format = "parquet",
    basename_template = "od-{i}.parquet"
  )

  archivos_parquet <- list.files(temporal, pattern = "[.]parquet$", full.names = TRUE)
  if (!length(archivos_parquet)) {
    stop(sprintf("La OD no contiene registros para %s.", fecha), call. = FALSE)
  }

  od_dia <- arrow::open_dataset(archivos_parquet, format = "parquet")
  controles <- resumir_od_ingerida(od_dia, columnas_clave)
  if (as.numeric(controles$resumen$filas[[1]]) == 0) {
    stop(sprintf("La OD no contiene registros para %s.", fecha), call. = FALSE)
  }

  controles$resumen$fecha_objetivo <- fecha
  controles$resumen$zona_horaria_operativa <- zona_horaria
  controles$resumen <- controles$resumen[c(
    "fecha_objetivo", "zona_horaria_operativa", "filas", "filas_global",
    "filas_valid", "personas", "personas_global", "fecha_min", "fecha_max",
    "lineas_subida", "variantes_subida", "paradas_subida", "paradas_bajada"
  )]

  data.table::fwrite(controles$resumen, file.path(temporal, "resumen_ingesta.csv"))
  data.table::fwrite(controles$faltantes, file.path(temporal, "faltantes_columnas_clave.csv"))

  info_fuente <- file.info(archivo_od)
  manifiesto <- list(
    fecha_objetivo = fecha,
    zona_horaria_operativa = zona_horaria,
    fuente = list(
      archivo = normalizePath(archivo_od, winslash = "/", mustWork = TRUE),
      bytes = unname(info_fuente$size),
      modificado = format(info_fuente$mtime, "%Y-%m-%dT%H:%M:%S%z")
    ),
    salida = list(
      formato = "parquet",
      filas = as.numeric(controles$resumen$filas[[1]]),
      archivos = basename(archivos_parquet)
    ),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  yaml::write_yaml(manifiesto, file.path(temporal, "manifiesto.yml"))

  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover la salida temporal a: %s", destino), call. = FALSE)
  }
  terminado <- TRUE

  list(destino = destino, resumen = controles$resumen, faltantes = controles$faltantes)
}

