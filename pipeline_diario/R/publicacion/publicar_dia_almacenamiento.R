materializar_directorio_persistente <- function(
    ubicacion,
    destino_cache,
    cliente_s3 = NULL) {
  if (!grepl("^s3[a-z0-9+.-]*://", ubicacion)) {
    if (!dir.exists(ubicacion)) {
      stop(sprintf("No existe el directorio persistente: %s.", ubicacion),
        call. = FALSE)
    }
    return(normalizePath(ubicacion, winslash = "/", mustWork = TRUE))
  }
  if (is.null(cliente_s3)) {
    stop("Se requiere un cliente S3 para materializar el directorio.",
      call. = FALSE)
  }
  partes <- descomponer_uri_s3(ubicacion)
  descargar_prefijo_s3(
    cliente = cliente_s3,
    prefijo_relativo = "",
    destino_local = destino_cache,
    bucket = partes$bucket,
    prefix = partes$prefix,
    reutilizar_existente = TRUE
  )
  normalizePath(destino_cache, winslash = "/", mustWork = TRUE)
}

publicar_dia_almacenamiento <- function(
    fecha,
    directorio_ocupacion,
    directorio_insumos,
    directorio_regiones,
    directorio_red_canonica,
    published_root,
    backend,
    work_root,
    cliente_s3 = NULL) {
  if (identical(backend, "local")) {
    return(publicar_dia(
      fecha = fecha,
      directorio_ocupacion = directorio_ocupacion,
      directorio_insumos = directorio_insumos,
      directorio_regiones = directorio_regiones,
      directorio_red_canonica = directorio_red_canonica,
      directorio_publicado = published_root
    ))
  }
  if (!identical(backend, "s3")) {
    stop(sprintf("Backend de publicacion desconocido: %s.", backend),
      call. = FALSE)
  }
  if (is.null(cliente_s3)) {
    stop("Se requiere un cliente S3 para publicar.", call. = FALSE)
  }
  ubicacion <- descomponer_uri_s3(published_root)
  catalogo_relativo <- unir_key_s3(
    "catalogo", paste0("fecha=", fecha, ".yml")
  )
  if (existe_objeto_s3(
      cliente_s3, catalogo_relativo,
      bucket = ubicacion$bucket, prefix = ubicacion$prefix)) {
    stop(sprintf("El catalogo S3 ya contiene la fecha %s.", fecha),
      call. = FALSE)
  }

  dir.create(work_root, recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile("publicacion_", tmpdir = work_root)
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  resultado <- publicar_dia(
    fecha = fecha,
    directorio_ocupacion = directorio_ocupacion,
    directorio_insumos = directorio_insumos,
    directorio_regiones = directorio_regiones,
    directorio_red_canonica = directorio_red_canonica,
    directorio_publicado = staging
  )

  plan_referencias <- subir_directorio_s3(
    cliente_s3,
    directorio_local = file.path(staging, "referencias"),
    prefijo_relativo = "referencias",
    bucket = ubicacion$bucket,
    prefix = ubicacion$prefix,
    permitir_existentes_identicos = TRUE
  )
  relativo_dia <- unir_key_s3("dias", paste0("fecha=", fecha))
  plan_dia <- subir_directorio_s3(
    cliente_s3,
    directorio_local = file.path(staging, "dias", paste0("fecha=", fecha)),
    prefijo_relativo = relativo_dia,
    bucket = ubicacion$bucket,
    prefix = ubicacion$prefix,
    permitir_existentes_identicos = TRUE
  )
  archivo_catalogo <- file.path(
    staging, "catalogo", paste0("fecha=", fecha, ".yml")
  )
  key_catalogo <- subir_archivo_s3(
    cliente_s3,
    ruta_local = archivo_catalogo,
    ruta_relativa = catalogo_relativo,
    bucket = ubicacion$bucket,
    prefix = ubicacion$prefix,
    sobrescribir = FALSE
  )
  metadatos_catalogo <- obtener_metadatos_objeto_s3(
    cliente_s3, catalogo_relativo,
    bucket = ubicacion$bucket, prefix = ubicacion$prefix
  )
  if (!identical(
      metadatos_catalogo$bytes,
      as.numeric(file.info(archivo_catalogo)$size))) {
    stop("El catalogo S3 no supero la verificacion de tamanio.",
      call. = FALSE)
  }
  resultado$destino <- paste0(
    sub("/+$", "", published_root), "/", relativo_dia
  )
  resultado$archivo_catalogo <- key_catalogo
  resultado$transferencia <- list(
    referencias = plan_referencias,
    dia = plan_dia,
    catalogo = key_catalogo
  )
  resultado
}
