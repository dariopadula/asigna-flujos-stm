argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
raiz_proyecto <- normalizePath(file.path(dirname(normalizePath(archivo_script)), "..", ".."), winslash = "/")
if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
configuracion <- yaml::read_yaml(file.path(raiz_proyecto, "config", "referencias_202605.yml"))
resolver <- function(ruta) normalizePath(file.path(raiz_proyecto, ruta), winslash = "/", mustWork = FALSE)
source(file.path(raiz_proyecto, "pipeline_referencias", "R", "fuentes", "preparar_fuentes_constructor.R"))
rutas <- lapply(configuracion$canonical_constructor$sources[c(
  "urban_streets", "historical_stops", "montevideo_boundary", "osm_gpkg"
)], resolver)
controles <- validar_fuentes_constructor(rutas, configuracion$reference_snapshot$metric_crs)
print(controles, row.names = FALSE)
if (any(!controles$aprobado)) stop("Las fuentes del constructor no aprobaron los controles.", call. = FALSE)
