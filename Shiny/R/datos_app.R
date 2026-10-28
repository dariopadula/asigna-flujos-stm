cargar_datos_app <- function(
    raiz_proyecto, configuracion = NULL, cliente_s3 = NULL) {
  if (is.null(configuracion)) {
    source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"),
      local = TRUE)
    configuracion <- cargar_configuracion(raiz_proyecto)
  }
  publicacion <- configuracion$publication
  if (is.null(publicacion$output_root) ||
      is.null(publicacion$presentation_registry_version)) {
    stop("La configuracion de publicacion esta incompleta.", call. = FALSE)
  }
  if (es_uri_objeto(publicacion$output_root)) {
    if (!exists("materializar_base_publicada_s3", mode = "function") ||
        !exists("obtener_cliente_s3", mode = "function")) {
      stop("No se cargaron las utilidades de almacenamiento S3.",
        call. = FALSE)
    }
    if (is.null(cliente_s3)) cliente_s3 <- obtener_cliente_s3()
    return(materializar_base_publicada_s3(
      raiz_publicada = publicacion$output_root,
      cache_root = configuracion$execution$cache_root,
      version_presentacion = publicacion$presentation_registry_version,
      cliente_s3 = cliente_s3
    ))
  }
  cargar_base_shiny_publicada(
    raiz_publicada = publicacion$output_root,
    version_presentacion = publicacion$presentation_registry_version
  )
}

# Los modulos conservan estos nombres durante la migracion para no alterar su
# contrato funcional. La aplicacion autocontenida tiene una unica fuente:
# publicaciones limpias con catalogo listo.
load_beta_data <- function(root, configuracion = NULL, cliente_s3 = NULL) {
  cargar_datos_app(root, configuracion, cliente_s3)
}

get_beta_day_data <- function(data, date) {
  cargar_dia_shiny_publicado(data, date)
}

