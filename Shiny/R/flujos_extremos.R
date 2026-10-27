polygon_feature_to_sf <- function(feature) {
  if (!is.null(feature$type) && identical(feature$type,"FeatureCollection")) {
    if (is.null(feature$features) || !length(feature$features)) return(NULL)
    feature <- feature$features[[1]]
  }
  if (is.null(feature) || is.null(feature$geometry$coordinates)) return(NULL)
  type <- feature$geometry$type
  coords <- feature$geometry$coordinates
  if (identical(type, "Polygon")) rings <- coords else if (identical(type, "MultiPolygon")) rings <- coords[[1]] else return(NULL)
  matrices <- lapply(rings, function(ring) matrix(as.numeric(unlist(ring)), ncol = 2, byrow = TRUE))
  sf::st_sfc(sf::st_polygon(matrices), crs = 4326)
}

has_spatial_features <- function(shape) {
  !is.null(shape) && NROW(shape) > 0
}

select_zone_stop_ids <- function(data, zone_type, zone_values = character(), polygon = NULL) {
  if (identical(zone_type, "poligono")) {
    if (is.null(polygon)) return(integer())
    stops_sf <- sf::st_as_sf(as.data.frame(data$stops), coords = c("lon", "lat"), crs = 4326)
    return(data$stops$COD_UBIC_P[lengths(sf::st_intersects(stops_sf, polygon)) > 0])
  }
  column <- c(
    barrio = "BARRIO", ccz = "CCZ", municipio = "MUNICIPIO",
    departamento = "DEPARTAMENTO"
  )[[zone_type]]
  if (is.null(column) || !length(zone_values)) return(integer())
  unique(data$stop_regions[get(column) %in% zone_values, COD_UBIC_P])
}

select_destination_stop_ids <- select_zone_stop_ids

query_selected_flow <- function(data, date, hours, origin_stop_ids = NULL,
    destination_stop_ids = NULL, time_reference = c("subida", "bajada"),
    other_scope = c("all", "inside", "outside"), scope_stop_ids = NULL) {
  time_reference <- match.arg(time_reference)
  other_scope <- match.arg(other_scope)
  time_column <- if (time_reference == "subida") "tiempo_subida" else "tiempo_bajada"
  trips <- filter_operational_window(data$trips, date, hours, time_column)
  if (!is.null(origin_stop_ids)) {
    trips <- trips[COD_PARADA_ORIGEN_EXPANSION %in% origin_stop_ids]
  }
  if (!is.null(destination_stop_ids)) {
    trips <- trips[COD_PARADA_DESTINO_EXPANSION %in% destination_stop_ids]
  }
  if (other_scope != "all") {
    stopifnot(length(scope_stop_ids) > 0)
    if (!is.null(origin_stop_ids) && is.null(destination_stop_ids)) {
      inside <- trips$COD_PARADA_DESTINO_EXPANSION %in% scope_stop_ids
    } else if (is.null(origin_stop_ids) && !is.null(destination_stop_ids)) {
      inside <- trips$COD_PARADA_ORIGEN_EXPANSION %in% scope_stop_ids
    } else {
      stop("El filtro Dentro/Fuera requiere una consulta de un solo extremo.")
    }
    trips <- trips[if (other_scope == "inside") inside else !inside]
  }
  origin <- trips[, .(personas = sum(cantidad_pasajeros)), by = .(COD_UBIC_P = COD_PARADA_ORIGEN_EXPANSION)]
  destination <- trips[, .(personas = sum(cantidad_pasajeros)), by = .(COD_UBIC_P = COD_PARADA_DESTINO_EXPANSION)]
  origin <- merge(origin, data$stops, by = "COD_UBIC_P", all.x = TRUE)
  destination <- merge(destination, data$stops, by = "COD_UBIC_P", all.x = TRUE)
  if (!nrow(trips)) return(list(trips = trips, flow = data.table::data.table(), origin = origin, destination = destination))
  bus <- data$bus[recorrido %in% unique(trips$recorrido)]
  occupied <- calculate_occupancy(bus, trips)
  if (exists("aggregate_beta_positions", mode = "function")) {
    flow <- aggregate_beta_positions(data, occupied, "personas")
  } else {
    flow <- occupied[, .(personas = sum(ocupacion_personas), buses = sum(buses)), by = ID_punto]
    flow <- merge(flow[personas > 0], data$points, by = "ID_punto", all.x = TRUE)
  }
  list(trips = trips, flow = flow, origin = origin, destination = destination)
}

query_destination_flow <- function(data, date, hours, stop_ids,
    other_scope = c("all", "inside", "outside")) {
  other_scope <- match.arg(other_scope)
  query_selected_flow(data, date, hours, destination_stop_ids = stop_ids,
    time_reference = "bajada", other_scope = other_scope, scope_stop_ids = stop_ids)
}

query_origin_flow <- function(data, date, hours, stop_ids,
    other_scope = c("all", "inside", "outside")) {
  other_scope <- match.arg(other_scope)
  query_selected_flow(data, date, hours, origin_stop_ids = stop_ids,
    time_reference = "subida", other_scope = other_scope, scope_stop_ids = stop_ids)
}

query_od_flow <- function(data, date, hours, origin_stop_ids, destination_stop_ids,
    time_reference = c("subida", "bajada")) {
  query_selected_flow(data, date, hours, origin_stop_ids, destination_stop_ids,
    time_reference)
}

