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
    "Uso: Rscript pipeline_diario/scripts/12_estimar_horarios_recorridos.R AAAA-MM-DD",
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
fecha <- argumentos[[1]]
intermedio <- resolver_ruta(configuracion$paths$intermediate)
registro <- file.path(
  resolver_ruta(configuracion$daily_variant_usage$canonical_registry_root),
  paste0("red=", configuracion$daily_variant_usage$canonical_network_version),
  paste0("entrega=", configuracion$daily_variant_usage$reference_snapshot)
)
source(file.path(
  raiz_proyecto, "pipeline_diario", "R", "horarios",
  "estimar_horarios_recorridos.R"
))

resultado <- estimar_horarios_recorridos(
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
  directorio_registro_canonico = registro,
  directorio_salida = intermedio,
  zona_horaria = configuracion$project$timezone,
  recorridos_por_lote = configuracion$schedules$routes_per_batch,
  velocidad_maxima_kmh = configuracion$schedules$max_edge_speed_kmh,
  ajuste_maximo_s = configuracion$schedules$max_anchor_adjustment_s
)
cat(sprintf("Horarios terminados: %s\n", resultado$destino))
print(resultado$resumen, row.names = FALSE)
cat(sprintf(
  "Velocidad global de respaldo: %.2f km/h\n",
  resultado$velocidad_respaldo_kmh
))
cat(sprintf(
  "Controles aprobados: %s | fallidos: %s\n",
  sum(resultado$controles$aprobado),
  sum(!resultado$controles$aprobado)
))
