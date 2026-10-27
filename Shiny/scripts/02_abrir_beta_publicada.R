detectar_raiz_proyecto <- function() {
  archivo_origen <- tryCatch(
    normalizePath(sys.frame(1L)$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )
  candidatas <- c(
    ".",
    if (!is.na(archivo_origen)) file.path(dirname(archivo_origen), "..", "..")
  )
  for (candidata in candidatas) {
    if (file.exists(file.path(candidata, "config", "base.yml"))) {
      return(normalizePath(candidata, winslash = "/", mustWork = TRUE))
    }
  }
  stop(
    "No se encontro la raiz autocontenida de proyecto_flujo.",
    call. = FALSE
  )
}

abrir_app_publicada <- function(
    host = Sys.getenv("ASIGNA_FLUJOS_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("ASIGNA_FLUJOS_PORT", "43818")),
    abrir_navegador = interactive()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Falta instalar el paquete R 'shiny'.", call. = FALSE)
  }
  raiz_proyecto <- detectar_raiz_proyecto()
  shiny::runApp(
    file.path(raiz_proyecto, "Shiny"),
    host = host,
    port = port,
    launch.browser = abrir_navegador
  )
}

abrir_app_publicada()
rm(abrir_app_publicada, detectar_raiz_proyecto)

