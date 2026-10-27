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
  raiz_proyecto, "pipeline_referencias", "R", "canonica",
  "migrar_red_canonica_inicial.R"
))
origen <- resolver_ruta(configuracion$paths$canonical_bootstrap_source)

resultado <- migrar_red_canonica_inicial(
  version_red = "canonica_202605_v1",
  archivo_corredores = file.path(origen, "catalogo_corredores.rds"),
  archivo_puntos = file.path(origen, "catalogo_puntos.rds"),
  directorio_registro = resolver_ruta(configuracion$paths$canonical_registry),
  corredores_esperados = configuracion$expected_initial$canonical_corridors,
  puntos_esperados = configuracion$expected_initial$canonical_points,
  crs_esperado = configuracion$reference_snapshot$metric_crs
)
cat(sprintf("Red canonica inicial migrada: %s\n", resultado$destino))
print(resultado$manifiesto_archivos, row.names = FALSE)
