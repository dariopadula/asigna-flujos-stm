validar_particion_od_ingestada <- function(
    directorio,
    fecha,
    columnas_requeridas = character()) {
  fecha <- format(validar_fecha_iso(fecha), "%Y-%m-%d")
  if (!dir.exists(directorio)) {
    stop(sprintf("No existe la ingesta de %s: %s.", fecha, directorio),
      call. = FALSE)
  }
  partes <- sort(list.files(
    directorio, pattern = "[.]parquet$", full.names = TRUE, recursive = TRUE
  ))
  if (!length(partes)) {
    stop(sprintf("La ingesta de %s no contiene partes Parquet.", fecha),
      call. = FALSE)
  }
  dataset <- arrow::open_dataset(partes, format = "parquet")
  verificar_columnas(names(dataset$schema), columnas_requeridas)
  filas <- dplyr::collect(dplyr::summarise(dataset, filas = dplyr::n()))$filas
  if (is.na(filas) || as.numeric(filas) < 1) {
    stop(sprintf("La ingesta de %s no contiene filas.", fecha), call. = FALSE)
  }
  raiz <- normalizePath(directorio, winslash = "/", mustWork = TRUE)
  partes_normalizadas <- normalizePath(
    partes, winslash = "/", mustWork = TRUE
  )
  inventario <- data.frame(
    archivo = substring(partes_normalizadas, nchar(raiz) + 2L),
    bytes = as.numeric(file.info(partes)$size),
    md5 = unname(tools::md5sum(partes)),
    stringsAsFactors = FALSE
  )
  invisible(list(
    directorio = raiz,
    fecha = fecha,
    filas = as.numeric(filas),
    partes = partes,
    inventario = inventario
  ))
}

materializar_od_dia <- function(
    fecha,
    ingested_root,
    backend,
    cache_root,
    columnas_requeridas = character(),
    cliente_s3 = NULL) {
  fecha <- format(validar_fecha_iso(fecha), "%Y-%m-%d")
  relativa <- file.path("od", paste0("fecha=", fecha))
  if (identical(backend, "local")) {
    directorio <- file.path(ingested_root, relativa)
  } else if (identical(backend, "s3")) {
    if (is.null(cliente_s3)) {
      stop("Se requiere un cliente S3 para materializar la ingesta.",
        call. = FALSE)
    }
    ubicacion <- descomponer_uri_s3(ingested_root)
    directorio <- file.path(
      cache_root, "ingested", "od", paste0("fecha=", fecha)
    )
    descargar_prefijo_s3(
      cliente = cliente_s3,
      prefijo_relativo = gsub("\\\\", "/", relativa),
      destino_local = directorio,
      bucket = ubicacion$bucket,
      prefix = ubicacion$prefix,
      reutilizar_existente = TRUE
    )
  } else {
    stop(sprintf("Backend de ingesta desconocido: %s.", backend),
      call. = FALSE)
  }
  validar_particion_od_ingestada(
    directorio, fecha, columnas_requeridas = columnas_requeridas
  )
  normalizePath(directorio, winslash = "/", mustWork = TRUE)
}

subir_od_dia_s3 <- function(
    fecha,
    directorio_particion,
    ingested_root,
    cliente_s3,
    columnas_requeridas = character()) {
  fecha <- format(validar_fecha_iso(fecha), "%Y-%m-%d")
  validacion <- validar_particion_od_ingestada(
    directorio_particion, fecha, columnas_requeridas = columnas_requeridas
  )
  ubicacion <- descomponer_uri_s3(ingested_root)
  plan <- subir_directorio_s3(
    cliente = cliente_s3,
    directorio_local = directorio_particion,
    prefijo_relativo = unir_key_s3("od", paste0("fecha=", fecha)),
    bucket = ubicacion$bucket,
    prefix = ubicacion$prefix,
    permitir_existentes_identicos = FALSE,
    archivos_locales = validacion$partes
  )
  if (!all(plan$estado == "subido")) {
    stop("La particion OD no quedo completamente subida.", call. = FALSE)
  }
  plan
}
