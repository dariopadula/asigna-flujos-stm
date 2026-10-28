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
  stop("Uso: Rscript pipeline_diario/scripts/02_preparar_od_operativa.R AAAA-MM-DD", call. = FALSE)
}
if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)

source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
configuracion <- cargar_configuracion(raiz_proyecto)
resolver_ruta <- function(ruta) resolver_ubicacion_configurada(ruta, raiz_proyecto)
source(file.path(raiz_proyecto, "R", "almacenamiento", "s3_io.R"))
source(file.path(
  raiz_proyecto, "R", "almacenamiento", "materializar_configuracion.R"
))
cliente_s3 <- if (identical(configuracion$storage$backend, "s3")) {
  obtener_cliente_s3()
} else {
  NULL
}
configuracion <- materializar_configuracion_lectura(
  configuracion, cliente_s3, "reference_root"
)
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "ingesta", "ingerir_od_dia.R"
))
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "ingesta", "materializar_od_dia.R"
))
source(file.path(raiz_proyecto, "pipeline_diario", "R", "ingesta", "preparar_od_operativa.R"))

particion_od <- materializar_od_dia(
  fecha = argumentos[[1]],
  ingested_root = configuracion$storage$ingested_root,
  backend = configuracion$storage$backend,
  cache_root = configuracion$execution$cache_root,
  columnas_requeridas = unlist(configuracion$ingestion$required_columns),
  cliente_s3 = cliente_s3
)

resultado <- preparar_od_operativa(
  fecha = argumentos[[1]],
  directorio_od_diaria = dirname(particion_od),
  archivo_catalogo_lineas = resolver_ruta(configuracion$paths$line_catalog),
  directorio_salida = resolver_ruta(configuracion$paths$intermediate),
  columnas_operativas = unlist(configuracion$operational_od$columns)
)
cat(sprintf("OD operativa terminada: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)

