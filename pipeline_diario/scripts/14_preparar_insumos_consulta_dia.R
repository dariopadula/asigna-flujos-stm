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
    "Uso: Rscript pipeline_diario/scripts/14_preparar_insumos_consulta_dia.R AAAA-MM-DD",
    call. = FALSE
  )
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
}

source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
configuracion <- cargar_configuracion(raiz_proyecto)
source(file.path(raiz_proyecto, "R", "almacenamiento", "s3_io.R"))
source(file.path(
  raiz_proyecto, "R", "almacenamiento", "materializar_configuracion.R"
))
if (identical(configuracion$storage$backend, "s3")) {
  configuracion <- materializar_configuracion_lectura(
    configuracion, obtener_cliente_s3(), "registry_root"
  )
}
resolver_ruta <- function(ruta) {
  resolver_ubicacion_configurada(ruta, raiz_proyecto)
}
fecha <- argumentos[[1L]]
intermedio <- resolver_ruta(configuracion$paths$intermediate)
red <- configuracion$daily_variant_usage$canonical_network_version
snapshot <- configuracion$daily_variant_usage$reference_snapshot
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "consultas",
  "preparar_insumos_consulta_dia.R"
))

resultado <- preparar_insumos_consulta_dia(
  fecha = fecha,
  archivo_od_operativa = file.path(
    intermedio, "od_operativa", paste0("fecha=", fecha),
    "od_operativa.parquet"
  ),
  directorio_resolucion_paso09 = file.path(
    intermedio, "od_paradas_canonicas", paste0("fecha=", fecha)
  ),
  directorio_recuperacion_paso10 = file.path(
    intermedio, "recuperacion_paradas", paste0("fecha=", fecha)
  ),
  directorio_recorridos_paso11 = file.path(
    intermedio, "recorridos_operativos", paste0("fecha=", fecha)
  ),
  directorio_uso_variantes_paso08 = file.path(
    intermedio, "uso_variantes", paste0("fecha=", fecha)
  ),
  directorio_ocupacion_paso13 = file.path(
    intermedio, "ocupacion_recorridos", paste0("fecha=", fecha)
  ),
  directorio_paradas_canonicas = file.path(
    resolver_ruta(
      configuracion$daily_variant_usage$canonical_registry_root
    ),
    paste0("red=", red), paste0("entrega=", snapshot)
  ),
  directorio_salida = intermedio,
  zona_horaria = configuracion$project$timezone
)
cat(sprintf("Insumos de consulta terminados: %s\n", resultado$destino))
print(resultado$resumen_actividad, row.names = FALSE)
print(resultado$resumen_viajes, row.names = FALSE)
cat(sprintf(
  "Controles aprobados: %s | fallidos: %s\n",
  sum(resultado$controles$aprobado),
  sum(!resultado$controles$aprobado)
))
