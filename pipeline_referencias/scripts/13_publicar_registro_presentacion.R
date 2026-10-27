argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub(
  "^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1]
)
if (is.na(archivo_script) || !nzchar(archivo_script)) {
  stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
}
raiz_proyecto <- normalizePath(
  file.path(dirname(normalizePath(archivo_script, mustWork = TRUE)), "..", ".."),
  winslash = "/", mustWork = TRUE
)
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
}
referencias <- yaml::read_yaml(file.path(
  raiz_proyecto, "config", "referencias_202605.yml"
))
source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
pipeline <- cargar_configuracion(raiz_proyecto)
presentacion <- referencias$presentation_registry
source(file.path(
  raiz_proyecto, "pipeline_referencias", "R", "presentacion",
  "publicar_registro_presentacion.R"
))
resultado <- publicar_registro_presentacion(
  directorio_registro = file.path(
    raiz_proyecto, presentacion$output_root,
    paste0("version=", presentacion$version)
  ),
  directorio_publicado = pipeline$publication$output_root
)
cat(sprintf(
  "Referencia de presentacion %s: %s\n",
  if (resultado$reutilizado) "ya publicada" else "publicada",
  resultado$destino
))
