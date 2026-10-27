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
regional <- configuracion$regional_registry
resolver_ruta <- function(ruta) {
  normalizePath(file.path(raiz_proyecto, ruta),
    winslash = "/", mustWork = FALSE)
}
archivo_paradas <- file.path(
  raiz_proyecto, "data", "registry", "variantes_canonicas",
  paste0("red=", regional$canonical_network_version),
  paste0("entrega=", regional$reference_snapshot),
  "paradas_proyectadas.parquet"
)
source(file.path(
  raiz_proyecto, "pipeline_referencias", "R", "regiones",
  "construir_registro_regiones.R"
))
resultado <- construir_registro_regiones(
  version_regiones = regional$version,
  archivo_paradas_canonicas = archivo_paradas,
  archivo_barrios = resolver_ruta(regional$sources$barrios),
  archivo_ccz = resolver_ruta(regional$sources$ccz),
  archivo_municipios = resolver_ruta(regional$sources$municipios),
  archivo_departamentos = resolver_ruta(regional$sources$departamentos),
  directorio_registro = resolver_ruta(regional$output_root),
  version_variantes = regional$variant_registry_version,
  snapshot_referencias = regional$reference_snapshot,
  red_canonica = regional$canonical_network_version,
  departamentos_objetivo = unlist(regional$target_departments),
  crs_departamentos = as.integer(regional$source_crs_departments)
)
cat(sprintf("Registro regional terminado: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)
cat(sprintf(
  "Controles aprobados: %s | fallidos: %s\n",
  sum(resultado$controles$aprobado),
  sum(!resultado$controles$aprobado)
))
