muestrear_universo_variantes <- function(archivo_universo, distancia_m = 50, crs_metrico = 32721L) {
  universo <- data.table::as.data.table(arrow::read_parquet(archivo_universo))
  requeridas <- c(
    "codigo_variante", "id_version_variante", "codigo_linea",
    "nombre_linea", "sentido", "geometria_wkt"
  )
  faltantes <- setdiff(requeridas, names(universo))
  if (length(faltantes)) stop("El universo no cumple el contrato para muestreo.", call. = FALSE)
  geometria <- sf::st_as_sfc(universo$geometria_wkt, crs = crs_metrico)
  variantes <- sf::st_sf(universo[, ..requeridas][, geometria_wkt := NULL], geometry = geometria)
  partes <- lapply(seq_len(nrow(variantes)), function(indice) {
    variante <- variantes[indice, ]
    longitud <- as.numeric(sf::st_length(variante))
    cantidad <- max(2L, ceiling(longitud / distancia_m) + 1L)
    puntos <- suppressWarnings(sf::st_cast(
      sf::st_line_sample(sf::st_geometry(variante), sample = seq(0, 1, length.out = cantidad)),
      "POINT"
    ))
    sf::st_sf(
      indice_muestra = seq_along(puntos),
      fraccion = seq(0, 1, length.out = length(puntos)),
      codigo_variante = variante$codigo_variante[[1]],
      id_version_variante = variante$id_version_variante[[1]],
      codigo_linea = variante$codigo_linea[[1]],
      nombre_linea = variante$nombre_linea[[1]],
      sentido = variante$sentido[[1]],
      geometry = puntos
    )
  })
  do.call(rbind, partes)
}

cargar_limite_montevideo <- function(archivo, crs_metrico = 32721L) {
  limite <- sf::st_read(archivo, quiet = TRUE)
  if (is.na(sf::st_crs(limite))) sf::st_crs(limite) <- crs_metrico
  limite <- sf::st_transform(limite, crs_metrico)
  sf::st_sf(ambito = "Montevideo", geometry = sf::st_union(sf::st_make_valid(limite)))
}

preparar_red_osm <- function(
    muestras,
    limite_montevideo,
    archivo_osm,
    buffer_m = 250,
    mosaico_m = 20000) {
  clases_excluidas <- c(
    "bridleway", "construction", "cycleway", "footway", "ladder", "path",
    "pedestrian", "proposed", "raceway", "steps"
  )
  muestra_capa <- sf::st_read(
    archivo_osm, query = "SELECT * FROM lines LIMIT 1", quiet = TRUE
  )
  columna_geometria <- attr(muestra_capa, "sf_column")
  if (is.null(columna_geometria) || !nzchar(columna_geometria)) {
    stop("No se pudo identificar la geometria de la capa OSM lines.", call. = FALSE)
  }
  consulta <- sprintf(
    paste(
      "SELECT osm_id, name, highway, z_order, other_tags, %s",
      "FROM lines WHERE highway IS NOT NULL AND highway NOT IN (%s)"
    ),
    sprintf('"%s"', gsub('"', '""', columna_geometria)),
    paste(sprintf("'%s'", clases_excluidas), collapse = ", ")
  )
  vias_osm <- sf::st_read(archivo_osm, query = consulta, quiet = TRUE)
  vias_osm <- sf::st_transform(vias_osm, sf::st_crs(muestras))
  vias_osm$oneway <- grepl(
    '"oneway"=>"(yes|1|-1|true)"', vias_osm$other_tags, ignore.case = TRUE
  )
  muestras$zona_red <- ifelse(
    lengths(sf::st_intersects(muestras, limite_montevideo)) > 0L,
    "montevideo", "exterior"
  )
  muestras$osm_id <- NA_character_
  muestras$osm_name <- NA_character_
  muestras$osm_highway <- NA_character_
  muestras$osm_oneway <- NA
  muestras$distancia_osm_m <- NA_real_
  vias_osm$fila_osm <- seq_len(nrow(vias_osm))
  indices_exteriores <- which(muestras$zona_red == "exterior")
  coordenadas <- sf::st_coordinates(muestras[indices_exteriores, ])
  mosaicos <- paste(
    floor(coordenadas[, "X"] / mosaico_m),
    floor(coordenadas[, "Y"] / mosaico_m),
    sep = "|"
  )
  filas_vias <- integer()
  for (mosaico in unique(mosaicos)) {
    posiciones <- which(mosaicos == mosaico)
    filas_muestra <- indices_exteriores[posiciones]
    locales <- coordenadas[posiciones, , drop = FALSE]
    caja <- c(
      xmin = min(locales[, "X"]) - buffer_m,
      ymin = min(locales[, "Y"]) - buffer_m,
      xmax = max(locales[, "X"]) + buffer_m,
      ymax = max(locales[, "Y"]) + buffer_m
    )
    candidatas <- suppressWarnings(sf::st_crop(vias_osm, caja))
    if (!nrow(candidatas)) stop(sprintf("No hay vias OSM alrededor del mosaico %s.", mosaico), call. = FALSE)
    cercanas <- sf::st_nearest_feature(muestras[filas_muestra, ], candidatas)
    muestras$osm_id[filas_muestra] <- as.character(candidatas$osm_id[cercanas])
    muestras$osm_name[filas_muestra] <- candidatas$name[cercanas]
    muestras$osm_highway[filas_muestra] <- candidatas$highway[cercanas]
    muestras$osm_oneway[filas_muestra] <- candidatas$oneway[cercanas]
    muestras$distancia_osm_m[filas_muestra] <- as.numeric(sf::st_distance(
      muestras[filas_muestra, ], candidatas[cercanas, ], by_element = TRUE
    ))
    filas_vias <- union(filas_vias, candidatas$fila_osm[cercanas])
  }
  vias_cercanas <- vias_osm[sort(filas_vias), ]
  vias_cercanas$fila_osm <- NULL
  if (!nrow(vias_cercanas)) stop("No se seleccionaron vias OSM exteriores.", call. = FALSE)
  calidad <- data.table::as.data.table(sf::st_drop_geometry(muestras))[zona_red == "exterior", .(
    muestras = .N,
    distancia_mediana_m = stats::median(distancia_osm_m),
    distancia_p95_m = stats::quantile(distancia_osm_m, 0.95),
    distancia_maxima_m = max(distancia_osm_m),
    proporcion_20m = mean(distancia_osm_m <= 20),
    vias_osm = data.table::uniqueN(osm_id)
  ), by = .(codigo_variante, nombre_linea, sentido)]
  list(muestras = muestras, vias = vias_cercanas, calidad = calidad)
}
