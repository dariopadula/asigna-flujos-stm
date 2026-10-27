cargar_datos_app <- function(raiz_proyecto, configuracion = NULL) {
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
    stop(
      "La lectura Shiny desde S3 se habilitara tras confirmar el acceso Cloudera.",
      call. = FALSE
    )
  }
  cargar_base_shiny_publicada(
    raiz_publicada = publicacion$output_root,
    version_presentacion = publicacion$presentation_registry_version
  )
}

# Los modulos conservan estos nombres durante la migracion para no alterar su
# contrato funcional. La aplicacion autocontenida tiene una unica fuente:
# publicaciones limpias con catalogo listo.
load_beta_data <- function(root, configuracion = NULL) {
  cargar_datos_app(root, configuracion)
}

get_beta_day_data <- function(data, date) {
  cargar_dia_shiny_publicado(data, date)
}

