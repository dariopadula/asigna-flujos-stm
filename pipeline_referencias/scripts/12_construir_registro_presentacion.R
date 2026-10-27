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
configuracion <- yaml::read_yaml(file.path(
  raiz_proyecto, "config", "referencias_202605.yml"
))
presentacion <- configuracion$presentation_registry
resolver_ruta <- function(ruta) {
  normalizePath(file.path(raiz_proyecto, ruta),
    winslash = "/", mustWork = FALSE)
}
directorio_variantes <- file.path(
  raiz_proyecto, "data", "registry", "variantes_canonicas",
  paste0("red=", presentacion$canonical_network_version),
  paste0("entrega=", presentacion$reference_snapshot)
)
archivo_puntos <- file.path(
  raiz_proyecto, "data", "registry", "canonica",
  paste0("red=", presentacion$canonical_network_version),
  "catalogo_puntos.rds"
)
source(file.path(
  raiz_proyecto, "pipeline_referencias", "R", "presentacion",
  "construir_registro_presentacion.R"
))
resultado <- construir_registro_presentacion(
  version_presentacion = presentacion$version,
  archivo_secuencias = file.path(directorio_variantes, "secuencias.parquet"),
  archivo_catalogo_puntos = archivo_puntos,
  archivo_referencia_orientacion_urbana = resolver_ruta(
    presentacion$sources$urban_orientation
  ),
  directorio_registro = resolver_ruta(presentacion$output_root),
  version_variantes = presentacion$variant_registry_version,
  snapshot_referencias = presentacion$reference_snapshot,
  red_canonica = presentacion$canonical_network_version,
  vias_sin_sentido = unlist(presentacion$streets_without_direction),
  desplazamiento_m = as.numeric(presentacion$map_offset_m),
  esperados = presentacion$expected_initial
)
cat(sprintf("Registro de presentacion terminado: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)
cat(sprintf(
  "Controles aprobados: %s | fallidos: %s\n",
  sum(resultado$controles$aprobado),
  sum(!resultado$controles$aprobado)
))
