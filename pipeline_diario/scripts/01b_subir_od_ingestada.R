argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub(
  "^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1L]
)
if (is.na(archivo_script) || !nzchar(archivo_script)) {
  stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
}
raiz_proyecto <- normalizePath(
  file.path(dirname(normalizePath(archivo_script, mustWork = TRUE)), "..", ".."),
  winslash = "/", mustWork = TRUE
)
argumentos <- commandArgs(trailingOnly = TRUE)
if (!length(argumentos) %in% 1:2) {
  stop(paste(
    "Uso: Rscript pipeline_diario/scripts/01b_subir_od_ingestada.R",
    "AAAA-MM-DD [RUTA_PARTICION_LOCAL]"
  ), call. = FALSE)
}
fecha <- argumentos[[1L]]
particion_local <- if (length(argumentos) == 2L) {
  argumentos[[2L]]
} else {
  file.path(raiz_proyecto, "data", "ingested", "od", paste0("fecha=", fecha))
}

source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
source(file.path(raiz_proyecto, "R", "almacenamiento", "s3_io.R"))
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "ingesta", "ingerir_od_dia.R"
))
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "ingesta", "materializar_od_dia.R"
))
configuracion <- cargar_configuracion(raiz_proyecto, "cloudera")
plan <- subir_od_dia_s3(
  fecha = fecha,
  directorio_particion = particion_local,
  ingested_root = configuracion$storage$ingested_root,
  cliente_s3 = obtener_cliente_s3(),
  columnas_requeridas = unlist(configuracion$ingestion$required_columns)
)
cat(sprintf(
  "Particion subida: %d archivos, %.2f MB.\n",
  nrow(plan), sum(plan$bytes) / 1024^2
))
