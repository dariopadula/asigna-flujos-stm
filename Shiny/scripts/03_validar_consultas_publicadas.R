argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub(
  "^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1]
)
if (is.na(archivo_script) || !nzchar(archivo_script)) {
  stop("Ejecute este control mediante Rscript.", call. = FALSE)
}
raiz <- normalizePath(
  file.path(dirname(normalizePath(archivo_script, mustWork = TRUE)), "..", ".."),
  winslash = "/", mustWork = TRUE
)

for (archivo in c(
    "Shiny/R/consultas_temporales.R",
    "Shiny/R/adaptador_datos_publicados.R",
    "Shiny/R/datos_app.R",
    "Shiny/R/ocupacion.R",
    "Shiny/R/actividad_paradas.R",
    "Shiny/R/flujos_extremos.R",
    "Shiny/R/consultas_flujo.R")) {
  source(file.path(raiz, archivo))
}

argumentos <- commandArgs(trailingOnly = TRUE)
fecha <- as.Date(if (length(argumentos)) argumentos[[1L]] else "2026-05-20")
horas <- c(7, 10)
cat("[consultas-publicadas] Cargando perfil y fecha...\n")
base <- load_beta_data(raiz)
dia <- get_beta_day_data(base, fecha)

pasadas_franja <- filter_operational_window(dia$bus, fecha, horas, "tiempo")
viajes_franja <- filter_operational_window(
  dia$trips, fecha, horas, "tiempo_subida"
)

flujo_general <- query_beta_flow(dia, fecha, horas, metric = "personas")

linea <- pasadas_franja[, .N, by = COD_LINEA][order(-N), COD_LINEA][1]
flujo_linea <- query_beta_flow(
  dia, fecha, horas, lines = linea, metric = "personas"
)

calle_elegida <- pasadas_franja[
  !is.na(street_key) & nzchar(street_key), .N,
  by = .(street_key, dirAv)
][order(-N)][1]
flujo_calle <- query_street_flow(
  dia, fecha, horas, metric = "personas",
  streets = calle_elegida$street_key,
  directions = calle_elegida$dirAv,
  display_mode = "street_only"
)

actividad <- query_stop_activity(dia, fecha, horas)

par_od <- viajes_franja[, .(
  personas = sum(cantidad_pasajeros)
), by = .(
  origen = COD_PARADA_ORIGEN_EXPANSION,
  destino = COD_PARADA_DESTINO_EXPANSION
)][order(-personas)][1]
flujo_od <- query_od_flow(
  dia, fecha, horas,
  origin_stop_ids = par_od$origen,
  destination_stop_ids = par_od$destino,
  time_reference = "subida"
)

resumen <- data.table::data.table(
  consulta = c(
    "flujo_general", "linea_mas_activa", "calle_sentido_mas_activo",
    "actividad_paradas", "par_od_mas_frecuente"
  ),
  seleccion = c(
    "toda_la_red", linea,
    paste(calle_elegida$street_key, calle_elegida$dirAv, sep = " | "),
    "toda_la_red", paste(par_od$origen, par_od$destino, sep = " -> ")
  ),
  registros = c(
    flujo_general$pasadas, flujo_linea$pasadas, flujo_calle$pasadas,
    nrow(actividad), nrow(flujo_od$trips)
  ),
  personas = c(
    sum(pasadas_franja$ocupacion_personas),
    sum(pasadas_franja[COD_LINEA == linea]$ocupacion_personas),
    flujo_calle$personas, sum(actividad$subidas),
    sum(flujo_od$trips$cantidad_pasajeros)
  )
)

controles <- data.table::data.table(
  control = c(
    "perfil_limpio", "fecha_en_cache", "flujo_general_no_vacio",
    "flujo_general_sin_coordenadas_faltantes", "filtro_linea_reduce_pasadas",
    "filtro_linea_coincide", "calle_sentido_no_vacio",
    "actividad_subidas_coincide", "actividad_bajadas_coincide",
    "par_od_no_vacio", "par_od_respeta_origen", "par_od_respeta_destino"
  ),
  aprobado = c(
    identical(dia$profile, "publicado_limpio"),
    exists(as.character(fecha), envir = base$daily_cache, inherits = FALSE),
    flujo_general$pasadas > 0,
    !anyNA(flujo_general$points[, .(lon, lat)]),
    flujo_linea$pasadas <= flujo_general$pasadas,
    flujo_linea$pasadas == nrow(pasadas_franja[COD_LINEA == linea]),
    flujo_calle$pasadas > 0 && nrow(flujo_calle$points) > 0,
    sum(actividad$subidas) == sum(
      filter_operational_window(dia$stop_passes, fecha, horas, "tiempo")$subidas
    ),
    sum(actividad$bajadas) == sum(
      filter_operational_window(dia$stop_passes, fecha, horas, "tiempo")$bajadas
    ),
    nrow(flujo_od$trips) > 0,
    all(flujo_od$trips$COD_PARADA_ORIGEN_EXPANSION == par_od$origen),
    all(flujo_od$trips$COD_PARADA_DESTINO_EXPANSION == par_od$destino)
  )
)

cat("[consultas-publicadas] Resumen 07:00-10:00:\n")
print(resumen, row.names = FALSE)
cat(sprintf(
  "Controles aprobados: %d | fallidos: %d\n",
  sum(controles$aprobado), sum(!controles$aprobado)
))
if (any(!controles$aprobado)) {
  print(controles[!aprobado], row.names = FALSE)
  stop("Fallaron consultas del perfil publicado.", call. = FALSE)
}

