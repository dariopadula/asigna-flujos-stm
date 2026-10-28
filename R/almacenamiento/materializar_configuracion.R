materializar_configuracion_lectura <- function(
    configuracion,
    cliente_s3 = NULL,
    raices = c("reference_root", "registry_root")) {
  if (!identical(configuracion$storage$backend, "s3")) {
    return(configuracion)
  }
  if (is.null(cliente_s3)) {
    stop("Se requiere un cliente S3 para materializar la configuracion.",
      call. = FALSE)
  }
  desconocidas <- setdiff(raices, names(configuracion$storage))
  if (length(desconocidas)) {
    stop(sprintf("Raices de configuracion desconocidas: %s.",
      paste(desconocidas, collapse = ", ")), call. = FALSE)
  }
  for (nombre in raices) {
    uri <- configuracion$storage[[nombre]]
    if (!es_uri_objeto(uri)) next
    ubicacion <- descomponer_uri_s3(uri)
    destino <- file.path(
      configuracion$execution$cache_root, "persistent",
      sub("_root$", "", nombre)
    )
    sincronizar_prefijo_s3(
      cliente_s3, "", destino,
      bucket = ubicacion$bucket, prefix = ubicacion$prefix
    )
    configuracion$storage[[nombre]] <- normalizePath(
      destino, winslash = "/", mustWork = TRUE
    )
  }
  materializar_compatibilidad_config(configuracion)
}
