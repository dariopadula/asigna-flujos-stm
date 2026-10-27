clasificar_similitud <- function(cobertura, rho_orden, p95_m, extremo_max_m,
                                 revision_m = 150, debil_m = 300) {
  orden_compatible <- is.na(rho_orden) || abs(rho_orden) >= 0.90
  if (is.finite(p95_m) && is.finite(extremo_max_m) &&
      p95_m <= revision_m && extremo_max_m <= debil_m &&
      cobertura >= 0.60 && orden_compatible) {
    return("alta")
  }
  if (is.finite(p95_m) && p95_m <= debil_m && cobertura >= 0.30) {
    return("parcial")
  }
  "baja"
}

comparar_variantes_misma_linea <- function(
    fecha, archivo_diagnostico, archivo_geometrias, archivos_paradas,
    directorio_salida, crs_metrico = 32721, revision_m = 150, debil_m = 300) {
  for (paquete in c("data.table", "sf", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  fecha_date <- as.Date(fecha)
  if (is.na(fecha_date) || format(fecha_date, "%Y-%m-%d") != fecha) {
    stop("La fecha debe tener formato AAAA-MM-DD.", call. = FALSE)
  }
  destino <- file.path(directorio_salida, "comparacion_variantes_misma_linea", paste0("fecha=", fecha))
  if (dir.exists(destino)) stop(sprintf("La comparacion ya existe: %s", destino), call. = FALSE)

  diagnostico <- data.table::fread(archivo_diagnostico)
  faltantes <- diagnostico[geometria_estado == "sin_wkt", .(
    codigo_faltante = as.character(codigo_variante),
    linea = as.character(lineas_subida_od),
    sentido_faltante = as.character(sentido_geometria),
    nombre_faltante = nombre_variante
  )]
  if (!nrow(faltantes)) stop("No hay variantes con WKT vacio para comparar.", call. = FALSE)

  anio_mes_objetivo <- as.integer(format(fecha_date, "%Y%m"))
  paradas <- data.table::rbindlist(lapply(archivos_paradas, function(path) {
    x <- data.table::fread(
      path, select = c("cod_ubic_parada", "cod_variante", "ordinal", "cod_linea", "anio_mes", "x", "y"),
      showProgress = FALSE
    )
    x[anio_mes == anio_mes_objetivo]
  }))
  paradas[, `:=`(
    variante = as.character(cod_variante),
    linea = as.character(cod_linea)
  )]
  paradas <- unique(paradas[linea %in% faltantes$linea],
    by = c("variante", "cod_ubic_parada", "ordinal", "x", "y"))

  geometrias <- data.table::fread(
    archivo_geometrias,
    select = c("codigo", "sentido", "nombre_variante", "fecha_vigencia", "fecha_baja", "the_geom"),
    showProgress = FALSE
  )
  geometrias[, `:=`(
    variante = as.character(codigo),
    vigencia = as.Date(substr(fecha_vigencia, 1L, 10L)),
    baja = as.Date(substr(fecha_baja, 1L, 10L))
  )]
  geometrias <- geometrias[variante %in% paradas$variante & !is.na(vigencia) & vigencia <= fecha_date]
  data.table::setorder(geometrias, variante, vigencia)
  geometrias <- geometrias[, .SD[.N], by = variante]
  geometrias <- geometrias[
    (is.na(baja) | baja >= fecha_date) & !is.na(the_geom) & nzchar(trimws(the_geom))
  ]
  geometrias_sf <- sf::st_as_sf(as.data.frame(geometrias), wkt = "the_geom", crs = crs_metrico)
  geometrias_sf <- geometrias_sf[!sf::st_is_empty(geometrias_sf) & sf::st_is_valid(geometrias_sf), ]

  resultados <- list()
  indice_resultado <- 0L
  for (i in seq_len(nrow(faltantes))) {
    f <- faltantes[i]
    paradas_f <- paradas[variante == f$codigo_faltante & is.finite(x) & is.finite(y)]
    candidatos <- unique(paradas[linea == f$linea & variante %in% geometrias_sf$variante, variante])
    if (!length(candidatos)) {
      indice_resultado <- indice_resultado + 1L
      resultados[[indice_resultado]] <- data.table::data.table(
        codigo_faltante = f$codigo_faltante, linea = f$linea,
        sentido_faltante = f$sentido_faltante, nombre_faltante = f$nombre_faltante,
        codigo_candidato = NA_character_, sentido_candidato = NA_character_,
        nombre_candidato = NA_character_, paradas_faltante = data.table::uniqueN(paradas_f$cod_ubic_parada),
        paradas_candidato = NA_integer_, paradas_compartidas = 0L,
        cobertura_paradas_faltante = 0, jaccard_paradas = 0,
        rho_orden = NA_real_, distancia_p50_m = NA_real_, distancia_p95_m = NA_real_,
        distancia_max_m = NA_real_, distancia_extremos_max_m = NA_real_,
        similitud = "sin_candidata"
      )
      next
    }

    puntos_f <- sf::st_transform(sf::st_as_sf(
      as.data.frame(paradas_f), coords = c("x", "y"), crs = 4326, remove = FALSE
    ), crs_metrico)
    for (codigo_candidato in candidatos) {
      paradas_c <- paradas[variante == codigo_candidato]
      ids_f <- unique(paradas_f$cod_ubic_parada)
      ids_c <- unique(paradas_c$cod_ubic_parada)
      compartidas <- intersect(ids_f, ids_c)
      cobertura <- length(compartidas) / max(1L, length(ids_f))
      jaccard <- length(compartidas) / max(1L, length(union(ids_f, ids_c)))

      orden_f <- paradas_f[ordinal > 0, .(ordinal_f = min(ordinal)), by = cod_ubic_parada]
      orden_c <- paradas_c[ordinal > 0, .(ordinal_c = min(ordinal)), by = cod_ubic_parada]
      orden <- merge(orden_f, orden_c, by = "cod_ubic_parada")
      rho <- if (nrow(orden) >= 3L) suppressWarnings(stats::cor(
        orden$ordinal_f, orden$ordinal_c, method = "spearman"
      )) else NA_real_

      geometria_c <- geometrias_sf[geometrias_sf$variante == codigo_candidato, ][1, ]
      distancias <- as.numeric(sf::st_distance(puntos_f, geometria_c))
      orden_extremos <- paradas_f[ordinal > 0][order(ordinal)]
      extremos <- if (nrow(orden_extremos) >= 2L) orden_extremos[c(1L, .N)] else paradas_f[c(1L, .N)]
      puntos_extremos <- sf::st_transform(sf::st_as_sf(
        as.data.frame(extremos), coords = c("x", "y"), crs = 4326
      ), crs_metrico)
      distancia_extremos <- max(as.numeric(sf::st_distance(puntos_extremos, geometria_c)))
      p95 <- as.numeric(stats::quantile(distancias, 0.95, na.rm = TRUE))
      similitud <- clasificar_similitud(
        cobertura, rho, p95, distancia_extremos, revision_m, debil_m
      )
      indice_resultado <- indice_resultado + 1L
      resultados[[indice_resultado]] <- data.table::data.table(
        codigo_faltante = f$codigo_faltante, linea = f$linea,
        sentido_faltante = f$sentido_faltante, nombre_faltante = f$nombre_faltante,
        codigo_candidato = codigo_candidato,
        sentido_candidato = as.character(geometria_c$sentido),
        nombre_candidato = as.character(geometria_c$nombre_variante),
        paradas_faltante = length(ids_f), paradas_candidato = length(ids_c),
        paradas_compartidas = length(compartidas), cobertura_paradas_faltante = cobertura,
        jaccard_paradas = jaccard, rho_orden = rho,
        distancia_p50_m = stats::median(distancias, na.rm = TRUE),
        distancia_p95_m = p95, distancia_max_m = max(distancias, na.rm = TRUE),
        distancia_extremos_max_m = distancia_extremos, similitud = similitud
      )
    }
  }
  comparaciones <- data.table::rbindlist(resultados, fill = TRUE)
  comparaciones[, prioridad := data.table::fcase(
    similitud == "alta", 1L, similitud == "parcial", 2L,
    similitud == "baja", 3L, default = 4L
  )]
  data.table::setorder(
    comparaciones, codigo_faltante, prioridad, distancia_p95_m,
    -cobertura_paradas_faltante, na.last = TRUE
  )
  mejores <- comparaciones[, .SD[1L], by = codigo_faltante]
  mejores[, prioridad := NULL]
  comparaciones[, prioridad := NULL]

  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(comparaciones, file.path(destino, "comparaciones.csv"))
  data.table::fwrite(mejores, file.path(destino, "mejor_candidata_por_variante.csv"))
  data.table::fwrite(mejores[, .(variantes = .N), by = similitud][order(similitud)],
    file.path(destino, "resumen.csv"))
  yaml::write_yaml(list(
    fecha = fecha,
    criterio_alta = sprintf("p95 <= %sm, extremos <= %sm, cobertura >= 60%% y orden compatible", revision_m, debil_m),
    criterio_parcial = sprintf("p95 <= %sm y cobertura >= 30%%", debil_m),
    uso = "semilla preliminar; nunca sustitucion automatica definitiva"
  ), file.path(destino, "manifiesto.yml"))
  list(destino = destino, mejores = mejores)
}

