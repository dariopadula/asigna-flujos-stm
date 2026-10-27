argumentos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) {
  stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
}
raiz_proyecto <- normalizePath(
  file.path(dirname(normalizePath(archivo_script)), "..", ".."),
  winslash = "/"
)
for (paquete in c("arrow", "data.table", "sf", "yaml")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
  }
}
configuracion <- yaml::read_yaml(file.path(
  raiz_proyecto, "config", "referencias_202605.yml"
))
source(file.path(
  raiz_proyecto, "pipeline_referencias", "R", "variantes",
  "construir_variantes_canonicas.R"
))
source(file.path(
  raiz_proyecto, "pipeline_referencias", "R", "variantes",
  "gestionar_variantes_canonicas.R"
))

ejecutar_asignacion_paradas_canonicas(raiz_proyecto, configuracion)
