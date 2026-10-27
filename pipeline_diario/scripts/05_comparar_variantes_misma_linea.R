argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
raiz_proyecto <- normalizePath(file.path(dirname(normalizePath(archivo_script)), "..", ".."), winslash = "/")
argumentos <- commandArgs(trailingOnly = TRUE)
if (length(argumentos) != 1L) stop("Uso: Rscript pipeline_diario/scripts/05_comparar_variantes_misma_linea.R AAAA-MM-DD", call. = FALSE)
source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
configuracion <- cargar_configuracion(raiz_proyecto)
resolver_ruta <- function(ruta) resolver_ubicacion_configurada(ruta, raiz_proyecto)
fecha <- argumentos[[1]]
source(file.path(raiz_proyecto, "pipeline_diario", "R", "variantes", "comparar_variantes_misma_linea.R"))
resultado <- comparar_variantes_misma_linea(
  fecha,
  file.path(resolver_ruta(configuracion$paths$intermediate), "diagnostico_variantes", paste0("fecha=", fecha), "diagnostico_variantes.csv"),
  resolver_ruta(configuracion$paths$variant_geometries),
  vapply(configuracion$paths$variant_stops, resolver_ruta, character(1)),
  resolver_ruta(configuracion$paths$intermediate),
  configuracion$project$metric_crs,
  configuracion$variant_similarity$review_distance_m,
  configuracion$variant_similarity$weak_distance_m
)
cat(sprintf("Comparacion terminada: %s\n", resultado$destino))
print(resultado$mejores, row.names = FALSE)

