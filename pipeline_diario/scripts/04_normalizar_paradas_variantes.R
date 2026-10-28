argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
raiz_proyecto <- normalizePath(
  file.path(dirname(normalizePath(archivo_script, mustWork = TRUE)), "..", ".."),
  winslash = "/", mustWork = TRUE
)
argumentos <- commandArgs(trailingOnly = TRUE)
if (length(argumentos) != 1L) {
  stop("Uso: Rscript pipeline_diario/scripts/04_normalizar_paradas_variantes.R AAAA-MM-DD", call. = FALSE)
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
source(file.path(raiz_proyecto, "pipeline_diario", "R", "variantes", "normalizar_paradas_variantes.R"))

resultado <- normalizar_paradas_variantes(
  fecha = fecha,
  archivo_diagnostico = file.path(
    resolver_ruta(configuracion$paths$intermediate), "diagnostico_variantes",
    paste0("fecha=", fecha), "diagnostico_variantes.csv"
  ),
  archivos_paradas = vapply(configuracion$paths$variant_stops, resolver_ruta, character(1)),
  directorio_salida = resolver_ruta(configuracion$paths$intermediate)
)
cat(sprintf("Normalizacion terminada: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)

