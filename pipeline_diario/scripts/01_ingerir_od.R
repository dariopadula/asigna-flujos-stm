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
sobrescribir <- "--sobrescribir" %in% argumentos
argumentos <- setdiff(argumentos, "--sobrescribir")
if (length(argumentos) != 1L) {
  stop(
    paste(
      "Uso: Rscript pipeline_diario/scripts/01_ingerir_od.R",
      "AAAA-MM-DD|AAAA-MM [--sobrescribir]"
    ),
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

if (es_uri_objeto(configuracion$storage$ingested_root) ||
    es_uri_objeto(configuracion$storage$source_root)) {
  stop(paste(
    "La normalizacion CSV se ejecuta sobre archivos locales.",
    "Genere la particion con el perfil local y subala luego a ingested S3."
  ), call. = FALSE)
}
es_mes <- grepl("^[0-9]{4}-[0-9]{2}$", argumentos[[1]])
fecha_fuente <- if (es_mes) paste0(argumentos[[1]], "-01") else argumentos[[1]]
if (es_mes) {
  resultado <- normalizar_od_mensual(
    archivo_od = resolver_fuente_od(configuracion, fecha_fuente),
    directorio_salida = resolver_ruta(configuracion$paths$ingested),
    columnas_requeridas = unlist(configuracion$ingestion$required_columns),
    columnas_clave = unlist(configuracion$ingestion$key_columns),
    mes = argumentos[[1]],
    zona_horaria = configuracion$project$timezone,
    sobrescribir = sobrescribir
  )
} else {
  resultado <- ingerir_od_dia(
    fecha = argumentos[[1]],
    archivo_od = resolver_fuente_od(configuracion, fecha_fuente),
    directorio_salida = resolver_ruta(configuracion$paths$ingested),
    columnas_requeridas = unlist(configuracion$ingestion$required_columns),
    columnas_clave = unlist(configuracion$ingestion$key_columns),
    zona_horaria = configuracion$project$timezone,
    sobrescribir = sobrescribir
  )
}

for (item in if (is.list(resultado) && !is.null(resultado$destino)) {
    list(resultado)
  } else {
    resultado
  }) {
  cat(sprintf("Ingesta terminada: %s\n", item$destino))
  print(item$resumen, row.names = FALSE)
}

