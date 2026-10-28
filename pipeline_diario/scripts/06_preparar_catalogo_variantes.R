argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
raiz_proyecto <- normalizePath(
  file.path(dirname(normalizePath(archivo_script, mustWork = TRUE)), "..", ".."),
  winslash = "/", mustWork = TRUE
)
argumentos <- commandArgs(trailingOnly = TRUE)
if (length(argumentos) != 1L) {
  stop("Uso: Rscript pipeline_diario/scripts/06_preparar_catalogo_variantes.R AAAA-MM-DD", call. = FALSE)
}
if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
configuracion <- cargar_configuracion(raiz_proyecto)
source(file.path(raiz_proyecto, "R", "almacenamiento", "s3_io.R"))
source(file.path(raiz_proyecto, "R", "almacenamiento", "materializar_configuracion.R"))
if (identical(configuracion$storage$backend, "s3")) {
  configuracion <- materializar_configuracion_lectura(
    configuracion, obtener_cliente_s3(), "reference_root"
  )
}
resolver_ruta <- function(ruta) resolver_ubicacion_configurada(ruta, raiz_proyecto)
fecha <- argumentos[[1]]
intermedio <- resolver_ruta(configuracion$paths$intermediate)
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "variantes",
  "preparar_catalogo_variantes_operativas.R"
))

resultado <- preparar_catalogo_variantes_operativas(
  fecha = fecha,
  archivo_diagnostico = file.path(
    intermedio, "diagnostico_variantes", paste0("fecha=", fecha),
    "diagnostico_variantes.csv"
  ),
  archivo_geometrias = resolver_ruta(configuracion$paths$variant_geometries),
  archivo_paradas_normalizadas = file.path(
    intermedio, "paradas_variantes_normalizadas", paste0("fecha=", fecha),
    "paradas_normalizadas.parquet"
  ),
  archivo_resumen_paradas = file.path(
    intermedio, "paradas_variantes_normalizadas", paste0("fecha=", fecha),
    "resumen_por_variante.csv"
  ),
  directorio_salida = intermedio
)
cat(sprintf("Catalogo terminado: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)
