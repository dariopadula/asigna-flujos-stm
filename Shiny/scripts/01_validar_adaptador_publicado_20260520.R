warning(
  paste0(
    "Este nombre se conserva por compatibilidad. Use ",
    "Shiny/scripts/01_validar_adaptador_publicado.R."
  ),
  call. = FALSE
)
archivo_actual <- normalizePath(sys.frame(1L)$ofile, mustWork = TRUE)
source(file.path(
  dirname(archivo_actual), "01_validar_adaptador_publicado.R"
))

