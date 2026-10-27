argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) {
  stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
}

raiz_proyecto <- normalizePath(
  file.path(dirname(normalizePath(archivo_script, mustWork = TRUE)), "..", ".."),
  winslash = "/",
  mustWork = TRUE
)

argumentos <- commandArgs(trailingOnly = TRUE)
if (length(argumentos) != 1L) {
  stop(
    "Uso: Rscript pipeline_diario/scripts/01_ingerir_od.R AAAA-MM-DD",
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

source(file.path(raiz_proyecto, "pipeline_diario", "R", "ingesta", "ingerir_od_dia.R"))

resultado <- ingerir_od_dia(
  fecha = argumentos[[1]],
  archivo_od = resolver_fuente_od(configuracion, argumentos[[1]]),
  directorio_salida = resolver_ruta(configuracion$paths$intermediate),
  columnas_requeridas = unlist(configuracion$ingestion$required_columns),
  columnas_clave = unlist(configuracion$ingestion$key_columns),
  zona_horaria = configuracion$project$timezone
)

cat(sprintf("Ingesta terminada: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)

