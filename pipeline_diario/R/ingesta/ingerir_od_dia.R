validar_fecha_iso <- function(fecha) {
  if (length(fecha) != 1L || is.na(fecha) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", fecha)) {
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
    stop(sprintf(
      "Faltan columnas requeridas en la OD: %s",
      paste(faltantes, collapse = ", ")
    ), call. = FALSE)
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
  arrow::open_dataset(
    archivo_od, format = "csv", schema = esquema, skip = 1L
  )
}

resumir_od_ingerida <- function(dataset, columnas_clave) {
  columnas_control <- unique(c(
    "fecha_evento_subida", "linea_codigo_subida", "sevar_codigo_subida",
    "codigo_parada_subida", "codigo_parada_bajada", "cantidad_pasajeros",
    "global", "valid", columnas_clave
  ))
  datos <- data.table::as.data.table(dplyr::collect(dplyr::select(
    dataset, dplyr::all_of(columnas_control)
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
      as.numeric(datos$cantidad_pasajeros[registros_globales]), na.rm = TRUE
    ),
    fecha_min = format(
      min(datos$fecha_evento_subida), "%Y-%m-%d %H:%M:%S", tz = "UTC"
    ),
    fecha_max = format(
      max(datos$fecha_evento_subida), "%Y-%m-%d %H:%M:%S", tz = "UTC"
    ),
    lineas_subida = data.table::uniqueN(
      datos$linea_codigo_subida, na.rm = TRUE
    ),
    variantes_subida = data.table::uniqueN(
      datos$sevar_codigo_subida, na.rm = TRUE
    ),
    paradas_subida = data.table::uniqueN(
      datos$codigo_parada_subida, na.rm = TRUE
    ),
    paradas_bajada = data.table::uniqueN(
      datos$codigo_parada_bajada, na.rm = TRUE
    ),
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

describir_esquema_arrow <- function(dataset) {
  campos <- dataset$schema$fields
  stats::setNames(
    vapply(campos, function(campo) campo$type$ToString(), character(1L)),
    names(dataset$schema)
  )
}

inventariar_archivos_locales <- function(directorio) {
  archivos <- list.files(directorio, recursive = TRUE, full.names = TRUE)
  archivos <- archivos[!dir.exists(archivos)]
  raiz <- normalizePath(directorio, winslash = "/", mustWork = TRUE)
  data.frame(
    archivo = substring(
      normalizePath(archivos, winslash = "/", mustWork = TRUE),
      nchar(raiz) + 2L
    ),
    bytes = as.numeric(file.info(archivos)$size),
    md5 = unname(tools::md5sum(archivos)),
    stringsAsFactors = FALSE
  )
}

escribir_particion_od_dia <- function(
    fuente,
    fecha,
    archivo_od,
    directorio_salida,
    columnas_clave,
    zona_horaria,
    sobrescribir = FALSE,
    filas_por_archivo = 1000000L,
    permitir_vacio = FALSE) {
  fecha_date <- validar_fecha_iso(fecha)
  destino <- file.path(directorio_salida, "od", paste0("fecha=", fecha))
  destino_existente <- dir.exists(destino) || file.exists(destino)
  if (destino_existente) {
    if (!isTRUE(sobrescribir)) {
      stop(sprintf("La ingesta ya existe y no sera sobrescrita: %s", destino),
        call. = FALSE)
    }
  }
  inicio <- as.POSIXct(fecha, tz = "UTC")
  fin <- as.POSIXct(fecha_date + 1L, tz = "UTC")
  seleccion <- dplyr::filter(
    fuente,
    .data$fecha_evento_subida >= inicio,
    .data$fecha_evento_subida < fin
  )

  padre_destino <- dirname(destino)
  dir.create(padre_destino, recursive = TRUE, showWarnings = FALSE)
  temporal <- tempfile(
    sprintf(".tmp_fecha_%s_", gsub("-", "", fecha)), tmpdir = padre_destino
  )
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) {
      unlink(temporal, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  filas_por_archivo <- as.integer(filas_por_archivo)
  if (is.na(filas_por_archivo) || filas_por_archivo < 1L) {
    stop("filas_por_archivo debe ser un entero positivo.", call. = FALSE)
  }
  arrow::write_dataset(
    seleccion,
    path = temporal,
    format = "parquet",
    basename_template = "part-{i}.parquet",
    max_rows_per_file = filas_por_archivo,
    max_rows_per_group = min(100000L, filas_por_archivo)
  )
  archivos_parquet <- sort(list.files(
    temporal, pattern = "[.]parquet$", full.names = TRUE
  ))
  if (!length(archivos_parquet)) {
    if (isTRUE(permitir_vacio)) return(NULL)
    stop(sprintf("La OD no contiene registros para %s.", fecha),
      call. = FALSE)
  }

  od_dia <- arrow::open_dataset(archivos_parquet, format = "parquet")
  controles <- resumir_od_ingerida(od_dia, columnas_clave)
  if (as.numeric(controles$resumen$filas[[1L]]) == 0) {
    if (isTRUE(permitir_vacio)) return(NULL)
    stop(sprintf("La OD no contiene registros para %s.", fecha),
      call. = FALSE)
  }
  controles$resumen$fecha_objetivo <- fecha
  controles$resumen$zona_horaria_operativa <- zona_horaria
  controles$resumen <- controles$resumen[c(
    "fecha_objetivo", "zona_horaria_operativa", "filas", "filas_global",
    "filas_valid", "personas", "personas_global", "fecha_min", "fecha_max",
    "lineas_subida", "variantes_subida", "paradas_subida", "paradas_bajada"
  )]
  data.table::fwrite(
    controles$resumen, file.path(temporal, "resumen_ingesta.csv")
  )
  data.table::fwrite(
    controles$faltantes,
    file.path(temporal, "faltantes_columnas_clave.csv")
  )
  inventario <- inventariar_archivos_locales(temporal)
  info_fuente <- file.info(archivo_od)
  manifiesto <- list(
    tipo = "od_normalizada_diaria",
    fecha_objetivo = fecha,
    zona_horaria_operativa = zona_horaria,
    fuente = list(
      formato = "csv_monthly",
      archivo = normalizePath(archivo_od, winslash = "/", mustWork = TRUE),
      bytes = unname(info_fuente$size),
      modificado = format(info_fuente$mtime, "%Y-%m-%dT%H:%M:%S%z")
    ),
    esquema = as.list(describir_esquema_arrow(od_dia)),
    salida = list(
      formato = "parquet",
      filas = as.numeric(controles$resumen$filas[[1L]]),
      personas = as.numeric(controles$resumen$personas[[1L]]),
      archivos_parquet = sum(grepl("[.]parquet$", inventario$archivo)),
      archivos = lapply(seq_len(nrow(inventario)), function(i) {
        list(
          archivo = inventario$archivo[[i]],
          bytes = inventario$bytes[[i]],
          md5 = inventario$md5[[i]]
        )
      })
    ),
    controles = list(
      columnas_requeridas = TRUE,
      filas_mayor_cero = TRUE,
      fecha_minima_en_dia = startsWith(
        controles$resumen$fecha_min[[1L]], fecha
      ),
      fecha_maxima_en_dia = startsWith(
        controles$resumen$fecha_max[[1L]], fecha
      ),
      faltantes_columnas_clave = as.numeric(
        sum(controles$faltantes$faltantes)
      )
    ),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  yaml::write_yaml(manifiesto, file.path(temporal, "manifiesto.yml"))
  respaldo <- NULL
  if (destino_existente) {
    respaldo <- tempfile(".respaldo_ingesta_", tmpdir = padre_destino)
    if (!file.rename(destino, respaldo)) {
      stop(sprintf("No se pudo apartar la ingesta existente: %s", destino),
        call. = FALSE)
    }
  }
  if (!file.rename(temporal, destino)) {
    if (!is.null(respaldo)) file.rename(respaldo, destino)
    stop(sprintf("No se pudo promover la salida temporal a: %s", destino),
      call. = FALSE)
  }
  if (!is.null(respaldo)) unlink(respaldo, recursive = TRUE, force = TRUE)
  terminado <- TRUE
  list(
    destino = destino,
    resumen = controles$resumen,
    faltantes = controles$faltantes,
    manifiesto = manifiesto
  )
}

normalizar_od_mensual <- function(
    archivo_od,
    directorio_salida,
    columnas_requeridas,
    columnas_clave,
    fechas = NULL,
    mes = NULL,
    zona_horaria = "America/Montevideo",
    sobrescribir = FALSE,
    filas_por_archivo = 1000000L) {
  for (paquete in c("arrow", "data.table", "dplyr", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete),
        call. = FALSE)
    }
  }
  if (!file.exists(archivo_od) || dir.exists(archivo_od)) {
    stop(sprintf("No existe el archivo OD: %s", archivo_od), call. = FALSE)
  }
  if (!is.null(fechas) && !is.null(mes)) {
    stop("Indique fechas o mes, no ambos.", call. = FALSE)
  }
  fuente <- abrir_od_csv(archivo_od)
  verificar_columnas(names(fuente$schema), columnas_requeridas)

  permitir_vacio <- FALSE
  if (!is.null(mes)) {
    if (length(mes) != 1L || is.na(mes) ||
        !grepl("^[0-9]{4}-[0-9]{2}$", mes)) {
      stop("El mes debe tener formato AAAA-MM.", call. = FALSE)
    }
    inicio <- as.Date(paste0(mes, "-01"))
    fin <- seq(inicio, by = "month", length.out = 2L)[[2L]] - 1L
    fechas <- format(seq(inicio, fin, by = "day"), "%Y-%m-%d")
    permitir_vacio <- TRUE
  }
  if (is.null(fechas) || !length(fechas)) {
    stop("Debe indicar al menos una fecha o un mes.", call. = FALSE)
  }
  fechas <- unique(vapply(fechas, function(x) {
    format(validar_fecha_iso(as.character(x)), "%Y-%m-%d")
  }, character(1L)))
  resultados <- lapply(fechas, function(fecha) {
    escribir_particion_od_dia(
      fuente = fuente,
      fecha = fecha,
      archivo_od = archivo_od,
      directorio_salida = directorio_salida,
      columnas_clave = columnas_clave,
      zona_horaria = zona_horaria,
      sobrescribir = sobrescribir,
      filas_por_archivo = filas_por_archivo,
      permitir_vacio = permitir_vacio
    )
  })
  resultados <- resultados[!vapply(resultados, is.null, logical(1L))]
  if (!length(resultados)) {
    stop("El CSV mensual no contiene fechas para el periodo solicitado.",
      call. = FALSE)
  }
  resultados
}

ingerir_od_dia <- function(
    fecha,
    archivo_od,
    directorio_salida,
    columnas_requeridas,
    columnas_clave,
    zona_horaria = "America/Montevideo",
    sobrescribir = FALSE,
    filas_por_archivo = 1000000L) {
  normalizar_od_mensual(
    archivo_od = archivo_od,
    directorio_salida = directorio_salida,
    columnas_requeridas = columnas_requeridas,
    columnas_clave = columnas_clave,
    fechas = fecha,
    zona_horaria = zona_horaria,
    sobrescribir = sobrescribir,
    filas_por_archivo = filas_por_archivo
  )[[1L]]
}
