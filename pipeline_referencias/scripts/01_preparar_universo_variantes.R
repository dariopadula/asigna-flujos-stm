argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
raiz_proyecto <- normalizePath(
  file.path(dirname(normalizePath(archivo_script, mustWork = TRUE)), "..", ".."),
  winslash = "/", mustWork = TRUE
)
if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
configuracion <- yaml::read_yaml(file.path(raiz_proyecto, "config", "referencias_202605.yml"))
resolver_ruta <- function(ruta) normalizePath(file.path(raiz_proyecto, ruta), winslash = "/", mustWork = FALSE)
source(file.path(
  raiz_proyecto, "pipeline_referencias", "R", "variantes",
  "preparar_universo_variantes.R"
))

resultado <- preparar_universo_variantes(
  snapshot_id = configuracion$reference_snapshot$id,
  fecha_corte = configuracion$reference_snapshot$cutoff_date,
  anio_mes = configuracion$reference_snapshot$year_month,
  subsistemas = unlist(configuracion$reference_snapshot$subsystem_codes),
  archivo_geometrias = resolver_ruta(configuracion$paths$variant_geometries),
  archivos_paradas = vapply(configuracion$paths$variant_stops, resolver_ruta, character(1)),
  archivo_catalogo_lineas = resolver_ruta(configuracion$paths$line_catalog),
  directorio_salida = resolver_ruta(configuracion$paths$intermediate_references),
  variantes_esperadas = configuracion$expected_initial$buildable_variants,
  paradas_esperadas = configuracion$expected_initial$universe_stops
)
cat(sprintf("Universo de referencias terminado: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)
