build_stop_activity <- function(bus, stop_index, trips) {
  library(data.table)
  index <- unique(as.data.table(stop_index)[,.(COD_VARIAN=as.character(COD_VARIAN),seqViajeN,COD_UBIC_P)])
  b <- as.data.table(bus)
  stopifnot(!anyDuplicated(b[,.(recorrido,seqViajeN)]))
  passes <- merge(b,index,by=c("COD_VARIAN","seqViajeN"),allow.cartesian=TRUE)
  passes <- unique(passes,by=c("recorrido","seqViajeN","COD_UBIC_P"))
  d <- as.data.table(trips)
  stopifnot(all(is.finite(d$cantidad_pasajeros)),all(d$cantidad_pasajeros>=0))
  up <- d[,.(subidas=sum(as.double(cantidad_pasajeros))),by=.(recorrido,seqViajeN=seq_origen_expansion,COD_UBIC_P=COD_PARADA_ORIGEN_EXPANSION)]
  down <- d[,.(bajadas=sum(as.double(cantidad_pasajeros))),by=.(recorrido,seqViajeN=seq_destino_expansion,COD_UBIC_P=COD_PARADA_DESTINO_EXPANSION)]
  keys <- passes[,.(recorrido,seqViajeN,COD_UBIC_P)]
  stopifnot(nrow(up[!keys,on=.(recorrido,seqViajeN,COD_UBIC_P)])==0,
    nrow(down[!keys,on=.(recorrido,seqViajeN,COD_UBIC_P)])==0)
  passes[,`:=`(subidas=0,bajadas=0)]
  passes[up,on=.(recorrido,seqViajeN,COD_UBIC_P),subidas:=i.subidas]
  passes[down,on=.(recorrido,seqViajeN,COD_UBIC_P),bajadas:=i.bajadas]
  stopifnot(sum(passes$subidas)==sum(d$cantidad_pasajeros),sum(passes$bajadas)==sum(d$cantidad_pasajeros))
  passes
}

query_stop_activity <- function(data,date,hours,lines=character(),
    service_type="todos") {
  p <- filter_operational_window(data$stop_passes,date,hours,"tiempo")
  p <- filter_service_type(p, service_type)
  if(length(lines)) p <- p[COD_LINEA %in% lines]
  result <- p[,.(subidas=sum(subidas),bajadas=sum(bajadas),buses=sum(buses)),by=COD_UBIC_P]
  merge(result,data$stops,by="COD_UBIC_P",all.x=TRUE)
}

query_street_stop_activity <- function(data, street_result) {
  runs <- street_result$eligible_runs
  trips <- data.table::as.data.table(street_result$trips)
  bus_stops <- data$stop_passes[recorrido %in% runs,
    .(buses=sum(as.double(buses))),by=COD_UBIC_P]
  if(nrow(trips)) {
    up <- trips[,.(subidas=sum(as.double(cantidad_pasajeros))),
      by=.(COD_UBIC_P=COD_PARADA_ORIGEN_EXPANSION)]
    down <- trips[,.(bajadas=sum(as.double(cantidad_pasajeros))),
      by=.(COD_UBIC_P=COD_PARADA_DESTINO_EXPANSION)]
  } else {
    empty_ids <- data.table::copy(data$stops[0,.(COD_UBIC_P)])
    up <- data.table::copy(empty_ids)[,subidas:=numeric()]
    down <- data.table::copy(empty_ids)[,bajadas:=numeric()]
  }
  result <- Reduce(function(x,y) merge(x,y,by="COD_UBIC_P",all=TRUE),
    list(up,down,bus_stops))
  for(column in c("subidas","bajadas","buses")) {
    if(!column %in% names(result)) result[,(column):=0]
    data.table::set(result,which(is.na(result[[column]])),column,0)
  }
  merge(result,data$stops,by="COD_UBIC_P",all.x=TRUE)
}

calculate_stop_concentration <- function(stops, metric, coverage = 0.8) {
  stopifnot(metric %in% c("subidas", "bajadas", "buses"), length(coverage) == 1,
    is.finite(coverage), coverage > 0, coverage <= 1)
  result <- data.table::copy(data.table::as.data.table(stops))
  result[, valor := as.numeric(get(metric))]
  result <- result[is.finite(valor) & valor > 0]
  data.table::setorder(result, -valor, COD_UBIC_P)
  if (!nrow(result)) {
    result[, `:=`(orden = integer(), porcentaje_paradas = numeric(),
      porcentaje_acumulado = numeric(), incluida = logical())]
    return(result)
  }
  result[, `:=`(
    orden = seq_len(.N),
    porcentaje_paradas = seq_len(.N) / .N,
    porcentaje_acumulado = cumsum(valor) / sum(valor)
  )]
  cutoff <- result[which(porcentaje_acumulado >= coverage)[1], valor]
  result[, incluida := valor >= cutoff]
  result
}

summarize_stop_concentration <- function(concentration) {
  selected <- concentration[incluida == TRUE]
  list(
    visible = nrow(selected),
    active = nrow(concentration),
    stop_percentage = if (nrow(concentration)) 100 * nrow(selected) / nrow(concentration) else 0,
    captured_percentage = if (nrow(selected)) 100 * max(selected$porcentaje_acumulado) else 0
  )
}

combine_endpoint_stop_activity <- function(origin, destination, stops) {
  origin <- data.table::as.data.table(origin)[,.(subidas=sum(personas)),by=COD_UBIC_P]
  destination <- data.table::as.data.table(destination)[,.(bajadas=sum(personas)),by=COD_UBIC_P]
  result <- merge(origin,destination,by="COD_UBIC_P",all=TRUE)
  result[is.na(subidas),subidas:=0]
  result[is.na(bajadas),bajadas:=0]
  metadata <- unique(data.table::as.data.table(stops)[,.(COD_UBIC_P,CALLE,lon,lat)],by="COD_UBIC_P")
  merge(result,metadata,by="COD_UBIC_P",all.x=TRUE)
}

