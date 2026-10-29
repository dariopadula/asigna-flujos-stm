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
for (paquete in c("arrow", "data.table", "sf", "yaml")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop(sprintf("Falta instalar el paquete R '%s'.", paquete),
      call. = FALSE)
  }
}
source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
configuracion <- cargar_configuracion(raiz_proyecto)
source(file.path(
  raiz_proyecto, "R", "almacenamiento", "s3_io.R"
))
source(file.path(
  raiz_proyecto, "Shiny", "R", "adaptador_datos_publicados.R"
))
source(file.path(
  raiz_proyecto, "Shiny", "R", "datos_app.R"
))

argumentos <- commandArgs(trailingOnly = TRUE)
fecha <- as.Date(if (length(argumentos)) argumentos[[1L]] else "2026-05-20")
cat("[shiny-publicado] Cargando catalogo y referencias estaticas...\n")
base <- cargar_datos_app(
  raiz_proyecto = raiz_proyecto,
  configuracion = configuracion
)
cat(sprintf(
  paste0(
    "[shiny-publicado] Referencias: %s ocurrencias, %s identidades, ",
    "%s paradas, %s calles.\n"
  ),
  format(nrow(base$occurrences), big.mark = ".", decimal.mark = ",",
    scientific = FALSE),
  format(nrow(base$presentation_points), big.mark = ".", decimal.mark = ",",
    scientific = FALSE),
  format(nrow(base$stops), big.mark = ".", decimal.mark = ",",
    scientific = FALSE),
  format(nrow(base$street_choices), big.mark = ".", decimal.mark = ",",
    scientific = FALSE)
))
cat(sprintf(
  "[shiny-publicado] Cargando las colecciones del %s...\n", fecha
))
dia <- cargar_dia_shiny_publicado(base, fecha)

resumen <- data.table::data.table(
  indicador = c(
    "fechas_listas", "ocurrencias_presentacion",
    "identidades_presentacion", "paradas", "calles",
    "filas_ocupacion", "recorridos", "filas_actividad",
    "registros_viajes", "personas", "subidas", "bajadas"
  ),
  valor = c(
    length(base$dates), nrow(base$occurrences),
    nrow(base$presentation_points), nrow(base$stops),
    nrow(base$street_choices), nrow(dia$bus),
    data.table::uniqueN(dia$bus$recorrido), nrow(dia$stop_passes),
    nrow(dia$trips), sum(dia$trips$cantidad_pasajeros),
    sum(dia$stop_passes$subidas), sum(dia$stop_passes$bajadas)
  )
)
esperados_diarios <- list(
  `2026-05-20` = c(
    filas_ocupacion = 6182199,
    recorridos = 20795,
    filas_actividad = 1450423,
    registros_viajes = 1155132,
    personas = 1180454,
    subidas = 1180454,
    bajadas = 1180454
  ),
  `2026-05-21` = c(
    filas_ocupacion = 6295926,
    recorridos = 21344,
    filas_actividad = 1482716,
    registros_viajes = 1177956,
    personas = 1202635,
    subidas = 1202635,
    bajadas = 1202635
  )
)
esperado_diario <- esperados_diarios[[as.character(fecha)]]
esperado <- c(
  ocurrencias_presentacion = 1069719,
  identidades_presentacion = 61273,
  paradas = 13250,
  calles = 2022
)
if (!is.null(esperado_diario)) esperado <- c(esperado, esperado_diario)
indicadores_regresion <- names(esperado)
controles <- data.table::data.table(
  control = c(
    "catalogo_no_vacio",
    "catalogo_contiene_fecha",
    paste0("regresion_", indicadores_regresion),
    "colecciones_diarias_no_vacias",
    "totales_diarios_consistentes",
    "ocupacion_con_presentacion_completa",
    "ocupacion_sin_claves_duplicadas",
    "viajes_con_intervalo_valido",
    "cache_diaria_activa"
  ),
  aprobado = c(
    length(base$dates) > 0L,
    fecha %in% base$dates,
    resumen$valor[match(indicadores_regresion, resumen$indicador)] == esperado,
    nrow(dia$bus) > 0L && nrow(dia$stop_passes) > 0L && nrow(dia$trips) > 0L,
    sum(dia$trips$cantidad_pasajeros) == sum(dia$stop_passes$subidas) &&
      sum(dia$trips$cantidad_pasajeros) == sum(dia$stop_passes$bajadas),
    !anyNA(dia$bus[, .(
      ID_presentacion, ID_punto, COD_VARIAN, COD_LINEA, dirAv
    )]),
    !anyDuplicated(dia$bus[, .(recorrido, seqViajeN)]),
    all(dia$trips$seq_destino_expansion > dia$trips$seq_origen_expansion),
    exists(as.character(fecha), envir = base$daily_cache, inherits = FALSE)
  )
)
controles[, estado := data.table::fifelse(
  aprobado, "aprobado", "fallo"
)]
cat("[shiny-publicado] Resumen de la carga real:\n")
print(resumen, row.names = FALSE)
cat(sprintf(
  "Controles aprobados: %s | fallidos: %s\n",
  sum(controles$aprobado), sum(!controles$aprobado)
))
if (any(!controles$aprobado)) {
  print(controles[!aprobado], row.names = FALSE)
  stop("La carga real del adaptador no aprobo sus controles.",
    call. = FALSE)
}

