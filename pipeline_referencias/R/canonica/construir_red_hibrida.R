extraer_tag_osm <- function(tags, clave) {
  patron <- sprintf('.*"%s"=>"([^"]+)".*', clave)
  coincide <- !is.na(tags) & grepl(patron, tags)
  resultado <- rep(NA_character_, length(tags))
  resultado[coincide] <- sub(patron, "\\1", tags[coincide])
  resultado
}

normalizar_texto_corredor <- function(valor) {
  resultado <- iconv(valor, from = "", to = "ASCII//TRANSLIT")
  resultado <- toupper(resultado)
  resultado <- gsub("[^A-Z0-9]+", "_", resultado)
  resultado <- gsub("^_+|_+$", "", resultado)
  resultado[!nzchar(resultado)] <- NA_character_
  resultado
}

componentes_conectados <- function(entidades, distancia_m) {
  vecinos <- sf::st_is_within_distance(entidades, entidades, dist = distancia_m)
  padre <- seq_len(nrow(entidades))
  raiz <- function(valor) {
    while (padre[[valor]] != valor) {
      padre[[valor]] <<- padre[[padre[[valor]]]]
      valor <- padre[[valor]]
    }
    valor
  }
  for (indice in seq_along(vecinos)) {
    for (candidato in vecinos[[indice]]) {
      primera <- raiz(indice)
      segunda <- raiz(candidato)
      if (primera != segunda) padre[[segunda]] <- primera
    }
  }
  raices <- vapply(seq_along(padre), raiz, integer(1))
  match(raices, unique(raices))
}

canonicalizar_corredor_exterior <- function(
    vias,
    corridor_id,
    nombre,
    distancia_puntos_m,
    p95_max_m,
    crs_metrico) {
  vias$NOM_CALLE <- nombre
  vias$COD_NOMBRE <- corridor_id
  if (nrow(vias) == 1L) {
    linea <- sf::st_sf(
      NOM_CALLE = nombre,
      COD_NOMBRE = corridor_id,
      geometry = sf::st_simplify(sf::st_geometry(vias), dTolerance = 2)
    )
    puntos <- muestrear_linea_canonica(linea, distancia_puntos_m, crs_metrico)
    if (nrow(puntos) == 1L) puntos <- suppressWarnings(sf::st_cast(linea, "POINT"))
    puntos$ID_calle <- seq_len(nrow(puntos))
    puntos$NOM_CALLE <- nombre
    puntos$COD_NOMBRE <- corridor_id
    return(list(linea = linea, puntos = puntos, metodo = "single_source"))
  }
  promediado <- simplificar_via_urbana(
    vias, distancia_puntos_m, celda_max_m = 250,
    partes_iniciales = 15, crs_metrico = crs_metrico
  )
  chequeo <- muestrear_linea_canonica(promediado$linea, 25, crs_metrico)
  distancias <- as.numeric(sf::st_distance(chequeo, sf::st_union(vias)))
  valido <- as.logical(sf::st_is_simple(promediado$linea)) &&
    stats::quantile(distancias, 0.95) <= p95_max_m
  if (valido) {
    promediado$metodo <- "grid_average"
    return(promediado)
  }
  partes <- suppressWarnings(sf::st_cast(sf::st_line_merge(sf::st_union(vias)), "LINESTRING"))
  seleccionada <- partes[which.max(as.numeric(sf::st_length(partes)))]
  linea <- sf::st_sf(
    NOM_CALLE = nombre,
    COD_NOMBRE = corridor_id,
    geometry = sf::st_simplify(seleccionada, dTolerance = 2)
  )
  puntos <- muestrear_linea_canonica(linea, distancia_puntos_m, crs_metrico)
  if (nrow(puntos) == 1L) puntos <- suppressWarnings(sf::st_cast(linea, "POINT"))
  puntos$ID_calle <- seq_len(nrow(puntos))
  puntos$NOM_CALLE <- nombre
  puntos$COD_NOMBRE <- corridor_id
  list(linea = linea, puntos = puntos, metodo = "source_axis_fallback")
}

construir_red_hibrida <- function(
    muestras,
    vias_osm,
    limite_montevideo,
    calles_urbanas,
    puntos_urbanos,
    version_red,
    archivo_osm_nombre,
    anio_mes,
    distancia_componente_m = 45,
    distancia_puntos_m = 75,
    corredor_p95_max_m = 40,
    crs_metrico = 32721L) {
  ids <- unique(muestras$osm_id[muestras$zona_red == "exterior" & !is.na(muestras$osm_id)])
  seleccion <- vias_osm[as.character(vias_osm$osm_id) %in% ids, ]
  seleccion$source_osm_id <- as.character(seleccion$osm_id)
  seleccion$osm_ref <- extraer_tag_osm(seleccion$other_tags, "ref")
  ref <- normalizar_texto_corredor(seleccion$osm_ref)
  nombre <- normalizar_texto_corredor(seleccion$name)
  seleccion$corridor_key <- ifelse(
    !is.na(ref), paste0("REF_", ref),
    ifelse(!is.na(nombre), paste0("NAME_", nombre), paste0("OSM_", seleccion$source_osm_id))
  )
  exteriores <- suppressWarnings(sf::st_difference(seleccion, sf::st_union(limite_montevideo)))
  exteriores <- exteriores[!sf::st_is_empty(exteriores), ]
  exteriores <- suppressWarnings(sf::st_cast(exteriores, "LINESTRING"))
  exteriores$corridor_component <- NA_integer_
  for (clave in unique(exteriores$corridor_key)) {
    indice <- which(exteriores$corridor_key == clave)
    exteriores$corridor_component[indice] <- componentes_conectados(
      exteriores[indice, ], distancia_componente_m
    )
  }
  exteriores$component_key <- paste(
    exteriores$corridor_key, exteriores$corridor_component, sep = "|"
  )
  metadatos <- data.table::rbindlist(lapply(
    split(seq_len(nrow(exteriores)), exteriores$component_key),
    function(indice) {
      actual <- exteriores[indice, ]
      centro <- sf::st_coordinates(sf::st_centroid(sf::st_union(actual)))[1, ]
      refs <- actual$osm_ref[!is.na(actual$osm_ref) & nzchar(actual$osm_ref)]
      nombres <- actual$name[!is.na(actual$name) & nzchar(actual$name)]
      data.table::data.table(
        component_key = actual$component_key[[1]],
        corridor_key = actual$corridor_key[[1]],
        grid_x = round(centro[["X"]] / 100),
        grid_y = round(centro[["Y"]] / 100),
        osm_ref = if (length(refs)) names(sort(table(refs), decreasing = TRUE))[[1]] else NA_character_,
        osm_name = if (length(nombres)) names(sort(table(nombres), decreasing = TRUE))[[1]] else NA_character_,
        source_way_count = data.table::uniqueN(actual$source_osm_id)
      )
    }
  ))
  metadatos[, corridor_id := paste0("EXT_", corridor_key, "_", grid_x, "_", grid_y)]
  exteriores$corridor_id <- metadatos$corridor_id[match(exteriores$component_key, metadatos$component_key)]

  lineas <- list()
  puntos <- list()
  metricas <- list()
  for (id in unique(exteriores$corridor_id)) {
    actual <- exteriores[exteriores$corridor_id == id, ]
    meta <- metadatos[corridor_id == id]
    resultado <- tryCatch(
      canonicalizar_corredor_exterior(
        actual, id, meta$osm_name[[1]], distancia_puntos_m,
        corredor_p95_max_m, crs_metrico
      ),
      error = function(e) e
    )
    if (inherits(resultado, "error")) {
      metricas[[id]] <- data.table::data.table(
        corridor_id = id, ok = FALSE, entidades_origen = nrow(actual),
        puntos_canonicos = NA_integer_, metodo = NA_character_, mensaje = conditionMessage(resultado)
      )
      next
    }
    linea <- resultado$linea
    names(linea)[names(linea) == "COD_NOMBRE"] <- "corridor_id"
    linea$corridor_key <- meta$corridor_key[[1]]
    linea$osm_ref <- meta$osm_ref[[1]]
    linea$osm_name <- meta$osm_name[[1]]
    linea$network_zone <- "exterior"
    linea$geometry_source <- archivo_osm_nombre
    linea$network_version <- version_red
    punto <- resultado$puntos
    names(punto)[names(punto) == "COD_NOMBRE"] <- "corridor_id"
    punto$corridor_point_index <- punto$ID_calle
    punto$canonical_point_id <- sprintf("%s_P%05d", id, punto$corridor_point_index)
    punto$corridor_key <- linea$corridor_key
    punto$osm_ref <- linea$osm_ref
    punto$osm_name <- linea$osm_name
    punto$network_zone <- "exterior"
    punto$geometry_source <- archivo_osm_nombre
    punto$network_version <- version_red
    punto$valid_from <- as.Date(sprintf("%s-%s-01", substr(anio_mes, 1, 4), substr(anio_mes, 5, 6)))
    punto$valid_to <- as.Date(NA)
    lineas[[id]] <- linea[, c(
      "corridor_id", "corridor_key", "osm_ref", "osm_name", "network_zone",
      "geometry_source", "network_version", "geometry"
    )]
    puntos[[id]] <- punto[, c(
      "canonical_point_id", "corridor_id", "corridor_key", "corridor_point_index",
      "osm_ref", "osm_name", "network_zone", "geometry_source", "network_version",
      "valid_from", "valid_to", "geometry"
    )]
    metricas[[id]] <- data.table::data.table(
      corridor_id = id, ok = TRUE, entidades_origen = nrow(actual),
      puntos_canonicos = nrow(punto), metodo = resultado$metodo, mensaje = "ok"
    )
  }
  metricas <- data.table::rbindlist(metricas, fill = TRUE)
  if (any(!metricas$ok)) stop("Algunos corredores exteriores no pudieron canonicalizarse.", call. = FALSE)
  lineas_exteriores <- do.call(rbind, lineas)
  puntos_exteriores <- do.call(rbind, puntos)

  lineas_urbanas <- sf::st_sf(
    corridor_id = paste0("MVD_", calles_urbanas$COD_NOMBRE),
    corridor_key = paste0("MVD_", calles_urbanas$COD_NOMBRE),
    osm_ref = NA_character_, osm_name = calles_urbanas$NOM_CALLE,
    network_zone = "montevideo", geometry_source = "base urbana reconstruida",
    network_version = version_red, geometry = sf::st_geometry(calles_urbanas)
  )
  puntos_urbanos_export <- sf::st_sf(
    canonical_point_id = paste0(
      "MVD_", puntos_urbanos$COD_NOMBRE, "_P", sprintf("%05d", puntos_urbanos$ID_calle)
    ),
    corridor_id = paste0("MVD_", puntos_urbanos$COD_NOMBRE),
    corridor_key = paste0("MVD_", puntos_urbanos$COD_NOMBRE),
    corridor_point_index = puntos_urbanos$ID_calle,
    osm_ref = NA_character_, osm_name = puntos_urbanos$NOM_CALLE,
    network_zone = "montevideo", geometry_source = "base urbana reconstruida",
    network_version = version_red, valid_from = as.Date(NA), valid_to = as.Date(NA),
    geometry = sf::st_geometry(puntos_urbanos)
  )
  catalogo_lineas <- rbind(lineas_urbanas, lineas_exteriores)
  catalogo_puntos <- rbind(puntos_urbanos_export, puntos_exteriores)
  if (anyDuplicated(catalogo_lineas$corridor_id)) stop("La red produjo corredores duplicados.", call. = FALSE)
  if (anyDuplicated(catalogo_puntos$canonical_point_id)) stop("La red produjo puntos duplicados.", call. = FALSE)
  if (!all(catalogo_puntos$corridor_id %in% catalogo_lineas$corridor_id)) {
    stop("La red produjo puntos sin corredor.", call. = FALSE)
  }
  list(
    corredores = catalogo_lineas,
    puntos = catalogo_puntos,
    vias_osm_seleccionadas = exteriores,
    calidad = metricas
  )
}
