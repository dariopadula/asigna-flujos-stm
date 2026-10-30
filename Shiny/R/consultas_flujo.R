prepare_query_passes <- function(data, passes) {
  result <- data.table::copy(data.table::as.data.table(passes))
  if ("ID_presentacion" %in% names(result)) return(result)
  if (!is.null(data$occurrences) && nrow(data$occurrences)) {
    result <- merge(
      result,
      data$occurrences,
      by = c("COD_VARIAN", "seqViajeN", "ID_punto"),
      all.x = TRUE,
      sort = FALSE
    )
    if (anyNA(result$ID_presentacion)) stop("La consulta contiene pasadas sin presentacion.", call. = FALSE)
    return(result)
  }
  result[, ID_presentacion := ID_punto]
  if (!"street_key" %in% names(result)) {
    lookup <- data.table::as.data.table(data$points)
    if (!"street_key" %in% names(lookup)) {
      lookup[, street_key := if ("NOM_CALLE" %in% names(lookup)) NOM_CALLE else ID_punto]
    }
    keep <- intersect(c("ID_punto", "street_key", "NOM_CALLE", "dirAv"), names(lookup))
    result <- merge(result, unique(lookup[, ..keep], by = "ID_punto"), by = "ID_punto", all.x = TRUE, sort = FALSE)
  }
  result
}

aggregate_beta_positions <- function(data, passes, metric = c("personas", "buses")) {
  metric <- match.arg(metric)
  prepared <- prepare_query_passes(data, passes)
  counts <- prepared[, .(
    personas = sum(as.double(ocupacion_personas)),
    buses = sum(as.double(buses))
  ), by = ID_presentacion]
  counts[, valor := if (metric == "personas") personas else buses]
  lookup <- data.table::copy(if (!is.null(data$presentation_points)) data$presentation_points else data$points)
  if (!"ID_presentacion" %in% names(lookup)) lookup[, ID_presentacion := ID_punto]
  result <- merge(counts, lookup, by = "ID_presentacion", all.x = TRUE)
  if (anyNA(result$ID_punto)) stop("La consulta genero identidades de presentacion sin geometria.", call. = FALSE)
  result[valor > 0]
}

query_beta_flow <- function(data, date, hours, lines=character(),
    metric="personas", service_type="todos") {
  stopifnot(length(hours)==2,all(is.finite(hours)),hours[1]>=0,hours[2]<=24,hours[2]>hours[1],metric %in% c("personas","buses"))
  b <- filter_operational_window(data$bus, date, hours, "tiempo")
  b <- filter_service_type(b, service_type)
  if(length(lines)) b <- b[COD_LINEA %in% lines]
  counts <- aggregate_beta_positions(data,b,metric)
  list(points=counts,recorridos=data.table::uniqueN(b$recorrido),pasadas=nrow(b),
    puntos=nrow(counts),maximo=if(nrow(counts)) max(counts$valor) else 0,
    extrapoladas=sum(b$metodo_tiempo %in% c("extrapolado_antes","extrapolado_despues","proyectado_velocidad_global")))
}

select_street_points <- function(data, streets, directions=character()) {
  if(!length(streets)) return(data.table::data.table())
  points <- if(!is.null(data$presentation_points)) data$presentation_points else data$points
  if(!"street_key" %in% names(points)) points[,street_key:=NOM_CALLE]
  selected <- points[street_key %in% streets]
  if(length(directions)) selected <- selected[dirAv %in% directions]
  selected
}

query_street_flow <- function(data,date,hours,lines=character(),metric="personas",streets,
    directions=character(),display_mode=c("street_only","associated"),match_mode=c("any","all"),
    service_type="todos") {
  display_mode <- match.arg(display_mode)
  match_mode <- match.arg(match_mode)
  stopifnot(length(streets)>0,metric %in% c("personas","buses"))
  selected_points <- select_street_points(data,streets,directions)
  if(!nrow(selected_points)) {
    return(list(points=data.table::data.table(),recorridos=0L,pasadas=0L,puntos=0L,maximo=0,
      extrapoladas=0L,street_points=selected_points,trips=data.table::data.table(),
      eligible_runs=character(),personas=0))
  }
  passes <- filter_operational_window(data$bus, date, hours, "tiempo")
  passes <- filter_service_type(passes, service_type)
  if(length(lines)) passes <- passes[COD_LINEA %in% lines]
  passes <- prepare_query_passes(data,passes)
  if(!"street_key" %in% names(passes)) {
    street_index <- unique(selected_points[,.(ID_punto,street_key,NOM_CALLE,dirAv)])
    passes <- merge(passes,street_index,by="ID_punto",all=FALSE,allow.cartesian=TRUE)
  }
  passes <- passes[street_key %in% streets]
  if(length(directions)) passes <- passes[dirAv %in% directions]
  passes <- unique(passes,by=c("recorrido","seqViajeN","street_key"))
  extrapolated <- c("extrapolado_antes","extrapolado_despues","proyectado_velocidad_global")
  if(!nrow(passes)) {
    return(list(points=data.table::data.table(),recorridos=0L,pasadas=0L,puntos=0L,maximo=0,
      extrapoladas=0L,street_points=selected_points,trips=data.table::data.table(),
      eligible_runs=character(),personas=0))
  }
  required <- if(match_mode=="all") length(unique(streets)) else 1L
  eligible_runs <- passes[,.(n_streets=data.table::uniqueN(street_key)),by=recorrido][n_streets>=required,recorrido]
  candidate_trips <- filter_service_type(
    data$trips, service_type
  )[recorrido %in% eligible_runs]
  hits <- merge(candidate_trips,
    passes[recorrido %in% eligible_runs,.(recorrido,seq_pass=seqViajeN,street_key)],
    by="recorrido",allow.cartesian=TRUE)
  hits <- hits[seq_origen_expansion<=seq_pass & seq_destino_expansion>seq_pass]
  eligible_ids <- hits[,.(n_streets=data.table::uniqueN(street_key)),by=trip_id][n_streets>=required,trip_id]
  selected_trips <- candidate_trips[trip_id %in% eligible_ids]
  if(display_mode=="street_only") {
    result <- query_beta_flow(
      data, date, hours, lines, metric, service_type
    )
    result$points <- aggregate_beta_positions(data,passes,metric)
    result$recorridos <- data.table::uniqueN(passes$recorrido)
    result$pasadas <- nrow(passes)
    result$puntos <- nrow(result$points)
    result$maximo <- if(nrow(result$points)) max(result$points$valor) else 0
    result$extrapoladas <- sum(passes$metodo_tiempo %in% extrapolated)
    result$street_points <- selected_points
    result$trips <- selected_trips
    result$eligible_runs <- eligible_runs
    result$personas <- sum(selected_trips$cantidad_pasajeros)
    return(result)
  }
  if(metric=="buses") {
    full_bus <- filter_service_type(
      data$bus, service_type
    )[recorrido %in% eligible_runs]
    points <- aggregate_beta_positions(data,full_bus,"buses")
    return(list(points=points,recorridos=length(eligible_runs),pasadas=nrow(passes[recorrido %in% eligible_runs]),
      puntos=nrow(points),maximo=if(nrow(points)) max(points$valor) else 0,
      extrapoladas=sum(passes[recorrido %in% eligible_runs,metodo_tiempo] %in% extrapolated),
      street_points=selected_points,trips=selected_trips,eligible_runs=eligible_runs,
      personas=sum(selected_trips$cantidad_pasajeros)))
  }
  if(!nrow(selected_trips)) {
    return(list(points=data.table::data.table(),recorridos=0L,pasadas=0L,puntos=0L,maximo=0,
      extrapoladas=0L,street_points=selected_points,trips=selected_trips,
      eligible_runs=eligible_runs,personas=0))
  }
  passenger_runs <- unique(selected_trips$recorrido)
  occupied <- calculate_occupancy(data$bus[recorrido %in% passenger_runs],selected_trips)
  points <- aggregate_beta_positions(data,occupied,"personas")
  list(points=points,recorridos=length(passenger_runs),pasadas=nrow(passes[recorrido %in% passenger_runs]),
    puntos=nrow(points),maximo=if(nrow(points)) max(points$valor) else 0,
    extrapoladas=sum(passes[recorrido %in% passenger_runs,metodo_tiempo] %in% extrapolated),
    street_points=selected_points,trips=selected_trips,eligible_runs=eligible_runs,
    personas=sum(selected_trips$cantidad_pasajeros))
}

