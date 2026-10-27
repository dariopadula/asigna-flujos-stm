# Ocupación de personas: cada registro pesa cantidad_pasajeros.
calculate_occupancy <- function(bus_positions, trips) {
  b <- data.table::copy(data.table::as.data.table(bus_positions))
  d <- data.table::as.data.table(trips)
  if (!"cantidad_pasajeros" %in% names(d) || !is.numeric(d$cantidad_pasajeros) ||
      any(!is.finite(d$cantidad_pasajeros) | d$cantidad_pasajeros < 0 |
          d$cantidad_pasajeros != floor(d$cantidad_pasajeros))) {
    stop("cantidad_pasajeros debe contener enteros no negativos, sin faltantes.")
  }
  stopifnot(!anyDuplicated(b[, .(recorrido, seqViajeN)]))
  data.table::setorder(b, recorrido, seqViajeN)
  d <- d[is.finite(seq_origen_expansion) & is.finite(seq_destino_expansion) &
    seq_destino_expansion > seq_origen_expansion]
  keys <- b[, .(recorrido, seqViajeN)]
  origins <- d[, .(recorrido, seqViajeN=seq_origen_expansion, peso=as.double(cantidad_pasajeros))]
  destinations <- d[, .(recorrido, seqViajeN=seq_destino_expansion, peso=as.double(cantidad_pasajeros))]
  stopifnot(nrow(origins[!keys,on=.(recorrido,seqViajeN)])==0L,
    nrow(destinations[!keys,on=.(recorrido,seqViajeN)])==0L)
  up <- origins[, .(subidas=sum(peso)), by=.(recorrido,seqViajeN)]
  down <- destinations[, .(bajadas=sum(peso)), by=.(recorrido,seqViajeN)]
  b[, `:=`(subidas=0,bajadas=0)]
  b[up,on=.(recorrido,seqViajeN),subidas:=i.subidas]
  b[down,on=.(recorrido,seqViajeN),bajadas:=i.bajadas]
  b[,ocupacion_personas:=cumsum(subidas-bajadas),by=recorrido]
  stopifnot(all(b$ocupacion_personas>=0L),sum(b$subidas)==sum(d$cantidad_pasajeros),sum(b$bajadas)==sum(d$cantidad_pasajeros),
    all(b[,.(final=tail(ocupacion_personas,1)),by=recorrido]$final==0L))
  b
}

