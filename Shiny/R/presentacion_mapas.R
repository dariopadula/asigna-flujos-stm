add_beta_base_tiles <- function(map) {
  map |>
    leaflet::addTiles(group = "OSM") |>
    leaflet::addTiles(
      urlTemplate = paste0(
        "https://montevideo.gub.uy/app/geowebcache/service/wmts/rest/",
        "mapstore-base:capas_base_puerta/default/",
        "EPSG:900913/EPSG:900913:{z}/{y}/{x}",
        "?format=image/jpeg"
      ),
      group = "Mapa IM"
    )
}

beta_base_groups <- function() c("Mapa IM", "OSM")

beta_flow_radius <- function(values) {
  values <- as.numeric(values)
  valid <- values[is.finite(values) & values > 0]
  if (!length(valid)) return(rep(1.5, length(values)))
  scaled <- values / max(valid)
  scaled[!is.finite(scaled)] <- 0
  1.5 + 3 * sqrt(pmin(pmax(scaled, 0), 1))
}

beta_stop_visual <- function(values) {
  values <- as.numeric(values)
  valid <- values[is.finite(values) & values > 0]
  if (!length(valid)) {
    return(list(top = 1, scaled = rep(0, length(values)), radius = rep(4, length(values)), opacity = rep(.55, length(values))))
  }
  top <- max(1, as.numeric(stats::quantile(
    valid, probs = .95, names = FALSE, type = 7
  )))
  scaled <- values / top
  scaled[!is.finite(scaled)] <- 0
  scaled <- pmin(pmax(scaled, 0), 1)
  list(
    top = top,
    scaled = scaled,
    radius = 4 + 7 * sqrt(scaled),
    opacity = .55 + .35 * sqrt(scaled)
  )
}

beta_stop_legend_title <- function(metric, top) {
  label <- switch(metric,
    subidas = "Subidas",
    bajadas = "Bajadas",
    buses = "Buses",
    metric
  )
  maximum <- format(top, big.mark = ".", decimal.mark = ",",
    scientific = FALSE, trim = TRUE)
  paste0(
    "Paradas &middot; ", label,
    "<br><span class='legend-detail'>M&aacute;x. color: ", maximum,
    "+ &middot; p95</span>"
  )
}

beta_stop_palette <- function(top) {
  palette <- leaflet::colorNumeric(
    c("#d9f0f0", "#67a9cf", "#2166ac", "#053061"),
    domain = c(0, top), na.color = "transparent"
  )
  # Los valores sobre el p95 conservan el azul máximo; fuera del dominio
  # colorNumeric los trataría como faltantes y quedarían transparentes.
  capped <- function(x) palette(pmin(pmax(as.numeric(x), 0), top))
  # addLegend utiliza estos atributos para reconocer la escala numérica.
  attributes(capped) <- attributes(palette)
  capped
}

