cargar_paradas_historicas_urbanas <- function(archivo, crs_origen = 4326L, crs_metrico = 32721L) {
  for (paquete in c("data.table", "sf")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  columnas <- c(
    "stm_paradas_por_variante_historico.cod_ubic_parada",
    "stm_paradas_por_variante_historico.cod_nombre_via_1",
    "stm_paradas_por_variante_historico.cod_nombre_via_2",
    "stm_paradas_por_variante_historico.the_geom",
    "stm_paradas_por_variante_historico.nom_calle",
    "stm_paradas_por_variante_historico.cod_variante",
    "stm_paradas_por_variante_historico.ordinal",
    "stm_paradas_por_variante_historico.cod_linea",
    "stm_paradas_por_variante_historico.desc_linea",
    "stm_paradas_por_variante_historico.rnk"
  )
  datos <- data.table::fread(archivo, select = columnas, showProgress = FALSE, encoding = "UTF-8")
  data.table::setnames(datos, sub("^stm_paradas_por_variante_historico[.]", "", names(datos)))
  data.table::setnames(datos, c(
    cod_ubic_parada = "COD_UBIC_P", cod_nombre_via_1 = "COD_CALLE1",
    cod_nombre_via_2 = "COD_CALLE2", nom_calle = "CALLE",
    cod_variante = "COD_VARIAN", ordinal = "ORDINAL",
    cod_linea = "id_linea", desc_linea = "DESC_LINEA", the_geom = "geometry"
  ))
  resultado <- sf::st_as_sf(as.data.frame(datos), wkt = "geometry", crs = crs_origen)
  resultado <- sf::st_transform(resultado, crs_metrico)
  coordenadas <- sf::st_coordinates(resultado)
  resultado$X <- coordenadas[, "X"]
  resultado$Y <- coordenadas[, "Y"]
  resultado
}

cargar_vias_urbanas <- function(archivo, crs_metrico = 32721L) {
  vias <- sf::st_read(archivo, quiet = TRUE, stringsAsFactors = FALSE, options = "ADJUST_TYPE=NO")
  if (!all(c("NOM_CALLE", "COD_NOMBRE") %in% names(vias))) {
    stop("La fuente de vias no contiene NOM_CALLE y COD_NOMBRE.", call. = FALSE)
  }
  if (is.na(sf::st_crs(vias))) sf::st_crs(vias) <- crs_metrico
  if (sf::st_crs(vias)$epsg != crs_metrico) vias <- sf::st_transform(vias, crs_metrico)
  vias
}

muestrear_linea_canonica <- function(linea, distancia_m, crs) {
  puntos <- sf::st_line_sample(linea, density = 1 / distancia_m)
  puntos <- suppressWarnings(sf::st_cast(puntos, "POINT"))
  sf::st_as_sf(data.frame(geometry = puntos), sf_column_name = "geometry", crs = crs)
}

orden_vecino_mas_cercano <- function(puntos, inicio = 1L) {
  if (nrow(puntos) <= 1L) return(seq_len(nrow(puntos)))
  distancias <- sf::st_distance(puntos, puntos)
  diag(distancias) <- Inf
  posicion <- inicio
  orden <- posicion
  distancias[, posicion] <- Inf
  while (length(orden) < nrow(puntos)) {
    posicion <- which.min(distancias[posicion, ])
    orden <- c(orden, posicion)
    distancias[, posicion] <- Inf
  }
  orden
}

inferir_direccion_via <- function(puntos, orientacion) {
  coordenadas <- sf::st_coordinates(puntos)
  varianzas <- prop.table(apply(coordenadas[, c("X", "Y"), drop = FALSE], 2, stats::var))
  revertir <- FALSE
  if (varianzas[["X"]] < varianzas[["Y"]]) {
    inicio <- "NORTE"
    fin <- "SUR"
    if (!identical(as.character(orientacion[["yRef"]]), "NORTE")) revertir <- TRUE
  } else {
    inicio <- "ESTE"
    fin <- "OESTE"
    if (!identical(as.character(orientacion[["xRef"]]), "ESTE")) revertir <- TRUE
  }
  list(inicio = inicio, fin = fin, revertir = revertir)
}

simplificar_via_urbana <- function(
    via,
    distancia_m = 75,
    celda_max_m = 500,
    partes_iniciales = 10,
    crs_metrico = 32721L) {
  if (!nrow(via)) stop("No se puede simplificar una via vacia.", call. = FALSE)
  caja <- sf::st_bbox(via)
  puntos_caja <- sf::st_as_sf(
    data.frame(
      X = rep(c(caja$xmin, caja$xmax), 2L),
      Y = rep(c(caja$ymin, caja$ymax), each = 2L)
    ),
    coords = c("X", "Y"), crs = crs_metrico
  )
  atributos <- unique(sf::st_drop_geometry(via)[, intersect(c("NOM_CALLE", "COD_NOMBRE"), names(via)), drop = FALSE])
  puntos_originales <- suppressWarnings(sf::st_cast(via, "POINT"))
  distancia_caja <- sf::st_distance(puntos_originales, puntos_caja)
  inicio <- which.max(rowSums(distancia_caja == min(distancia_caja)))[1]
  referencia_caja <- data.frame(
    pbox = 1:4,
    xRef = c("OESTE", "ESTE", "OESTE", "ESTE"),
    yRef = c("SUR", "SUR", "NORTE", "NORTE")
  )
  orientacion <- referencia_caja[which.min(distancia_caja[inicio, ]), ]
  punto_inicio <- puntos_originales[inicio, ]
  punto_fin <- puntos_originales[which.max(sf::st_distance(puntos_originales, punto_inicio)), ]

  celda <- min(as.numeric(max(sf::st_distance(puntos_caja, puntos_caja))) / partes_iniciales, celda_max_m)
  grilla <- sf::st_make_grid(via, cellsize = celda)
  while (length(grilla) <= 25L) {
    celda <- celda / 2
    grilla <- sf::st_make_grid(via, cellsize = celda)
  }
  grilla <- sf::st_as_sf(data.frame(geometry = grilla), sf_column_name = "geometry")
  coordenadas <- sf::st_coordinates(puntos_originales)
  puntos_originales <- cbind(puntos_originales, coordenadas[, c("X", "Y"), drop = FALSE])
  promedios <- stats::aggregate(
    puntos_originales[, c("X", "Y")], grilla, mean,
    join = function(x, y) sf::st_is_within_distance(x, y, dist = 0)
  )
  promedios <- promedios[!is.na(promedios$X), ]
  puntos_promedio <- sf::st_as_sf(sf::st_drop_geometry(promedios), coords = c("X", "Y"), crs = crs_metrico)
  incluye_inicio <- as.numeric(min(sf::st_distance(punto_inicio, puntos_promedio))) > 0
  incluye_fin <- as.numeric(min(sf::st_distance(punto_fin, puntos_promedio))) > 0
  if (incluye_inicio) puntos_promedio <- rbind(punto_inicio[, names(puntos_promedio)], puntos_promedio)
  if (incluye_fin) puntos_promedio <- rbind(puntos_promedio, punto_fin[, names(puntos_promedio)])
  orden <- orden_vecino_mas_cercano(puntos_promedio, 1L)
  if (!incluye_inicio) {
    distancia_promedios_caja <- sf::st_distance(puntos_promedio, puntos_caja)
    inicio_promedios <- which.max(rowSums(
      distancia_promedios_caja == min(distancia_promedios_caja)
    ))[1]
    orden <- orden_vecino_mas_cercano(puntos_promedio, inicio_promedios)
  }
  puntos_ordenados <- puntos_promedio[orden, ]
  linea_sfc <- sf::st_cast(sf::st_combine(sf::st_geometry(puntos_ordenados)), "LINESTRING")
  linea <- sf::st_sf(atributos, geometry = linea_sfc)
  puntos <- muestrear_linea_canonica(linea, distancia_m, crs_metrico)
  if (nrow(puntos) == 1L) puntos <- puntos_ordenados
  puntos$ID_calle <- seq_len(nrow(puntos))
  direccion <- inferir_direccion_via(puntos, orientacion)
  if (direccion$revertir) {
    puntos$ID_calle <- (nrow(puntos) + 1L) - puntos$ID_calle
    puntos <- puntos[order(puntos$ID_calle), ]
  }
  for (columna in names(atributos)) puntos[[columna]] <- atributos[[columna]][1]
  puntos$dirIni <- direccion$inicio
  puntos$dirFin <- direccion$fin
  list(linea = linea, puntos = puntos)
}

construir_base_urbana <- function(
    vias,
    paradas,
    distancia_m = 75,
    celda_max_m = 500,
    partes_iniciales = 10,
    crs_metrico = 32721L,
    verbose = TRUE) {
  codigos_usados <- unique(paradas$COD_CALLE1)
  vias_usadas <- vias[vias$COD_NOMBRE %in% codigos_usados, ]
  codigos <- unique(vias_usadas$COD_NOMBRE)
  lineas <- list()
  puntos <- list()
  metricas <- data.table::data.table(
    COD_NOMBRE = codigos, ok = FALSE,
    entidades_origen = NA_integer_, puntos_canonicos = NA_integer_, mensaje = NA_character_
  )
  for (indice in seq_along(codigos)) {
    codigo <- codigos[[indice]]
    if (verbose && indice %% 50L == 0L) message(sprintf("Calles procesadas: %s", indice))
    actual <- vias[vias$COD_NOMBRE == codigo, ]
    resultado <- tryCatch(
      simplificar_via_urbana(actual, distancia_m, celda_max_m, partes_iniciales, crs_metrico),
      error = function(e) e
    )
    metricas$entidades_origen[indice] <- nrow(actual)
    if (inherits(resultado, "error")) {
      metricas$mensaje[indice] <- conditionMessage(resultado)
      next
    }
    clave <- as.character(codigo)
    lineas[[clave]] <- resultado$linea
    puntos[[clave]] <- resultado$puntos
    metricas[indice, `:=`(ok = TRUE, puntos_canonicos = nrow(resultado$puntos), mensaje = "ok")]
  }
  if (any(!metricas$ok)) {
    stop("Algunas calles urbanas no pudieron canonicalizarse.", call. = FALSE)
  }
  indice_variante_calle <- unique(sf::st_drop_geometry(paradas)[, c(
    "DESC_LINEA", "COD_VARIAN", "CALLE", "COD_CALLE1"
  )])
  list(
    calles = do.call(rbind, lineas),
    puntos = do.call(rbind, puntos),
    metricas = metricas,
    indice_variante_calle = indice_variante_calle
  )
}
