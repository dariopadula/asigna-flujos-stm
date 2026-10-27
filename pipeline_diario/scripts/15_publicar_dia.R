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
argumentos <- commandArgs(trailingOnly = TRUE)
if (length(argumentos) != 1L) {
  stop(
    "Uso: Rscript pipeline_diario/scripts/15_publicar_dia.R AAAA-MM-DD",
    call. = FALSE
  )
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
}
source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
configuracion <- cargar_configuracion(raiz_proyecto)
resolver_ruta <- function(ruta) {
  resolver_ubicacion_configurada(ruta, raiz_proyecto)
}
fecha <- argumentos[[1L]]
intermedio <- resolver_ruta(configuracion$paths$intermediate)
red <- configuracion$daily_variant_usage$canonical_network_version
version_regiones <- configuracion$publication$regional_registry_version
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "publicacion", "publicar_dia.R"
))
resultado <- publicar_dia(
  fecha = fecha,
  directorio_ocupacion = file.path(
    intermedio, "ocupacion_recorridos", paste0("fecha=", fecha)
  ),
  directorio_insumos = file.path(
    intermedio, "insumos_consulta", paste0("fecha=", fecha)
  ),
  directorio_regiones = file.path(
    resolver_ruta(configuracion$publication$regional_registry_root),
    paste0("version=", version_regiones)
  ),
  directorio_red_canonica = file.path(
    configuracion$publication$canonical_network_root, paste0("red=", red)
  ),
  directorio_publicado = resolver_ruta(
    configuracion$publication$output_root
  )
)
cat(sprintf("Publicacion diaria terminada: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)
cat(sprintf(
  "Controles aprobados: %s | fallidos: %s\n",
  sum(resultado$controles$aprobado),
  sum(!resultado$controles$aprobado)
))
