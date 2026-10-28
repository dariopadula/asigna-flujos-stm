validar_particion_od_ingestada <- function(
    directorio,
    fecha,
    columnas_requeridas = character()) {
  fecha <- format(validar_fecha_iso(fecha), "%Y-%m-%d")
  manifiesto_archivo <- file.path(directorio, "manifiesto.yml")
  if (!dir.exists(directorio) || !file.exists(manifiesto_archivo)) {
    stop(sprintf("La ingesta de %s no contiene un manifiesto listo.", fecha),
      call. = FALSE)
  }
  manifiesto <- yaml::read_yaml(manifiesto_archivo)
  if (!identical(as.character(manifiesto$fecha_objetivo), fecha) ||
      !identical(as.character(manifiesto$salida$formato), "parquet")) {
    stop(sprintf("El manifiesto de ingesta no corresponde a %s.", fecha),
      call. = FALSE)
  }
  partes <- sort(list.files(
    directorio, pattern = "[.]parquet$", full.names = TRUE
  ))
  if (!length(partes)) {
    stop(sprintf("La ingesta de %s no contiene partes Parquet.", fecha),
      call. = FALSE)
  }
  dataset <- arrow::open_dataset(partes, format = "parquet")
  verificar_columnas(names(dataset$schema), columnas_requeridas)
  filas <- dplyr::collect(dplyr::summarise(dataset, filas = dplyr::n()))$filas
  if (!identical(as.numeric(filas), as.numeric(manifiesto$salida$filas))) {
    stop("Las filas Parquet no coinciden con el manifiesto de ingesta.",
      call. = FALSE)
  }
  archivos <- manifiesto$salida$archivos
  if (is.null(archivos) || !length(archivos)) {
    stop("El manifiesto de ingesta no contiene inventario de archivos.",
      call. = FALSE)
  }
  for (entrada in archivos) {
    ruta <- ruta_local_relativa(directorio, as.character(entrada$archivo))
    if (!file.exists(ruta) || dir.exists(ruta) ||
        !identical(as.numeric(file.info(ruta)$size), as.numeric(entrada$bytes)) ||
        !identical(unname(tools::md5sum(ruta)), as.character(entrada$md5))) {
      stop(sprintf("La ingesta contiene un archivo invalido: %s.",
        entrada$archivo), call. = FALSE)
    }
  }
  invisible(list(
    directorio = normalizePath(directorio, winslash = "/", mustWork = TRUE),
    manifiesto = manifiesto,
    partes = partes
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
  validar_particion_od_ingestada(
    directorio_particion, fecha, columnas_requeridas = columnas_requeridas
  )
  ubicacion <- descomponer_uri_s3(ingested_root)
  plan <- subir_directorio_s3(
    cliente = cliente_s3,
    directorio_local = directorio_particion,
    prefijo_relativo = unir_key_s3("od", paste0("fecha=", fecha)),
    bucket = ubicacion$bucket,
    prefix = ubicacion$prefix,
    permitir_existentes_identicos = FALSE
  )
  if (!all(plan$estado == "subido")) {
    stop("La particion OD no quedo completamente subida.", call. = FALSE)
  }
  plan
}
