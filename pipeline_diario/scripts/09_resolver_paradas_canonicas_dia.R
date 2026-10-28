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
    "Uso: Rscript pipeline_diario/scripts/09_resolver_paradas_canonicas_dia.R AAAA-MM-DD",
    call. = FALSE
  )
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
}

source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
configuracion <- cargar_configuracion(raiz_proyecto)
source(file.path(raiz_proyecto, "R", "almacenamiento", "s3_io.R"))
source(file.path(raiz_proyecto, "R", "almacenamiento", "materializar_configuracion.R"))
if (identical(configuracion$storage$backend, "s3")) {
  configuracion <- materializar_configuracion_lectura(
    configuracion, obtener_cliente_s3(), "registry_root"
  )
}
resolver_ruta <- function(ruta) {
  resolver_ubicacion_configurada(ruta, raiz_proyecto)
}
fecha <- argumentos[[1]]
intermedio <- resolver_ruta(configuracion$paths$intermediate)
registro <- file.path(
  resolver_ruta(configuracion$daily_variant_usage$canonical_registry_root),
  paste0("red=", configuracion$daily_variant_usage$canonical_network_version),
  paste0("entrega=", configuracion$daily_variant_usage$reference_snapshot)
)
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "paradas",
  "resolver_paradas_canonicas_dia.R"
))

resultado <- resolver_paradas_canonicas_dia(
  fecha = fecha,
  archivo_od_operativa = file.path(
    intermedio, "od_operativa", paste0("fecha=", fecha),
    "od_operativa.parquet"
  ),
  directorio_uso_variantes = file.path(
    intermedio, "uso_variantes", paste0("fecha=", fecha)
  ),
  directorio_registro_canonico = registro,
  directorio_salida = intermedio
)
cat(sprintf("Paradas canonicas del dia resueltas: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)
cat(sprintf(
  "Controles aprobados: %s | fallidos: %s\n",
  sum(resultado$controles$aprobado),
  sum(!resultado$controles$aprobado)
))
