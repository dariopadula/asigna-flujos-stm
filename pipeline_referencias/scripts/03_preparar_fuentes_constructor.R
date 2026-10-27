argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
raiz_proyecto <- normalizePath(file.path(dirname(normalizePath(archivo_script)), "..", ".."), winslash = "/")
if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
configuracion <- yaml::read_yaml(file.path(raiz_proyecto, "config", "referencias_202605.yml"))
resolver <- function(ruta) normalizePath(file.path(raiz_proyecto, ruta), winslash = "/", mustWork = FALSE)
source(file.path(raiz_proyecto, "pipeline_referencias", "R", "fuentes", "preparar_fuentes_constructor.R"))

origenes <- lapply(configuracion$canonical_constructor$migration_origins, resolver)
destinos <- lapply(configuracion$canonical_constructor$sources[names(origenes)], resolver)
manifesto <- preparar_fuentes_constructor(
  origenes, destinos,
  file.path(resolver(configuracion$canonical_constructor$sources$staged_root), "manifesto")
)
cat(sprintf("Fuentes preparadas: %s archivos\n", nrow(manifesto)))
