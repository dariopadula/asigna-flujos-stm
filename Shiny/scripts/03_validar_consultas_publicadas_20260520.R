warning(
  paste0(
    "Este nombre se conserva por compatibilidad. Use ",
    "Shiny/scripts/03_validar_consultas_publicadas.R."
  ),
  call. = FALSE
)
archivo_actual <- normalizePath(sys.frame(1L)$ofile, mustWork = TRUE)
source(file.path(
  dirname(archivo_actual), "03_validar_consultas_publicadas.R"
))

