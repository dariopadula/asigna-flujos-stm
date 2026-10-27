suavizar_corridas_cortas <- function(valores, maximo = 2L) {
  corridas <- rle(as.character(valores))
  if (length(corridas$values) < 3L) return(as.character(valores))
  inicios <- cumsum(c(1L, head(corridas$lengths, -1L)))
  finales <- cumsum(corridas$lengths)
  resultado <- as.character(valores)
  for (indice in 2:(length(corridas$values) - 1L)) {
    if (corridas$lengths[[indice]] <= maximo &&
        identical(corridas$values[[indice - 1L]], corridas$values[[indice + 1L]])) {
      resultado[inicios[[indice]]:finales[[indice]]] <- corridas$values[[indice - 1L]]
    }
  }
  resultado
}

asignar_zona_red <- function(muestras, limite_montevideo) {
  muestras$zona_red <- ifelse(
    lengths(sf::st_intersects(muestras, limite_montevideo)) > 0L,
    "montevideo", "exterior"
  )
  muestras
}

proyectar_muestras_zona <- function(
    muestras,
    corredores,
    puntos,
    maximo_corrida_corta = 2L,
    distancia_aceptada_m = 60,
    distancia_revision_m = 100) {
  if (!nrow(muestras)) return(muestras)
  if (!nrow(corredores) || !nrow(puntos)) {
    stop("Una zona tiene muestras pero no tiene catalogo canonico.", call. = FALSE)
  }
  muestras <- muestras[order(muestras$id_version_variante, muestras$indice_muestra), ]
  corredor_cercano <- sf::st_nearest_feature(muestras, corredores)
  muestras$corredor_inicial_id <- corredores$corridor_id[corredor_cercano]
  muestras$corredor_id <- muestras$corredor_inicial_id
  for (version in unique(muestras$id_version_variante)) {
    filas <- which(muestras$id_version_variante == version)
    muestras$corredor_id[filas] <- suavizar_corridas_cortas(
      muestras$corredor_id[filas], maximo_corrida_corta
    )
  }

  asignados <- integer(nrow(muestras))
  for (corredor in unique(muestras$corredor_id)) {
    filas_muestra <- which(muestras$corredor_id == corredor)
    filas_punto <- which(puntos$corridor_id == corredor)
    if (!length(filas_punto)) {
      stop(sprintf("El corredor %s no tiene puntos canonicos.", corredor), call. = FALSE)
    }
    cercanos <- sf::st_nearest_feature(muestras[filas_muestra, ], puntos[filas_punto, ])
    asignados[filas_muestra] <- filas_punto[cercanos]
  }
  muestras$punto_canonico_id <- puntos$canonical_point_id[asignados]
  muestras$distancia_canonica_m <- as.numeric(sf::st_distance(
    muestras, puntos[asignados, ], by_element = TRUE
  ))
  muestras$estado_coincidencia <- ifelse(
    muestras$distancia_canonica_m <= distancia_aceptada_m, "aceptada",
    ifelse(muestras$distancia_canonica_m <= distancia_revision_m, "revision", "atipica")
  )
  muestras
}

construir_secuencias_variantes <- function(
    muestras,
    limite_montevideo,
    corredores,
    puntos,
    maximo_corrida_corta = 2L,
    distancia_aceptada_m = 60,
    distancia_revision_m = 100,
    informar_progreso = FALSE,
    cada_variantes = 100L) {
  requeridas_muestras <- c(
    "codigo_variante", "id_version_variante", "indice_muestra", "codigo_linea",
    "nombre_linea", "sentido"
  )
  requeridas_catalogo <- c("corridor_id", "network_zone")
  if (length(setdiff(requeridas_muestras, names(muestras))) ||
      length(setdiff(requeridas_catalogo, names(corredores))) ||
      length(setdiff(c("canonical_point_id", requeridas_catalogo), names(puntos)))) {
    stop("Las entradas no cumplen el contrato de proyeccion canonica.", call. = FALSE)
  }
  if (anyDuplicated(puntos$canonical_point_id)) {
    stop("El catalogo contiene puntos canonicos duplicados.", call. = FALSE)
  }
  muestras <- asignar_zona_red(muestras, limite_montevideo)
  partes <- lapply(c("montevideo", "exterior"), function(zona) {
    muestras_zona <- muestras[muestras$zona_red == zona, ]
    versiones_zona <- unique(muestras_zona$id_version_variante)
    if (isTRUE(informar_progreso)) {
      cat(sprintf(
        "[proyeccion] Zona %s: %s muestras de %s variantes.\n",
        zona, nrow(muestras_zona), length(versiones_zona)
      ))
    }
    if (!length(versiones_zona)) return(NULL)
    lotes <- split(
      versiones_zona,
      ceiling(seq_along(versiones_zona) / cada_variantes)
    )
    resultados <- lapply(seq_along(lotes), function(indice_lote) {
      versiones_lote <- lotes[[indice_lote]]
      resultado <- proyectar_muestras_zona(
        muestras_zona[muestras_zona$id_version_variante %in% versiones_lote, ],
        corredores[corredores$network_zone == zona, ],
        puntos[puntos$network_zone == zona, ],
        maximo_corrida_corta, distancia_aceptada_m, distancia_revision_m
      )
      if (isTRUE(informar_progreso)) {
        cat(sprintf(
          "[proyeccion] Zona %s: lote %s/%s terminado (%s variantes).\n",
          zona, indice_lote, length(lotes), length(versiones_lote)
        ))
      }
      resultado
    })
    do.call(rbind, resultados)
  })
  diagnostico <- do.call(rbind, partes[!vapply(partes, is.null, logical(1))])
  diagnostico <- diagnostico[order(
    diagnostico$id_version_variante, diagnostico$indice_muestra
  ), ]

  grupos_diagnostico <- split(diagnostico, diagnostico$id_version_variante)
  secuencias <- do.call(rbind, lapply(
    seq_along(grupos_diagnostico),
    function(indice) {
      datos <- grupos_diagnostico[[indice]]
      if (isTRUE(informar_progreso) &&
          (indice %% cada_variantes == 0L || indice == length(grupos_diagnostico))) {
        cat(sprintf(
          "[secuencias] %s/%s variantes procesadas.\n",
          indice, length(grupos_diagnostico)
        ))
      }
      datos <- datos[order(datos$indice_muestra), ]
      conservar <- c(
        TRUE,
        datos$punto_canonico_id[-1L] != datos$punto_canonico_id[-nrow(datos)]
      )
      datos <- datos[conservar, ]
      datos$indice_secuencia <- seq_len(nrow(datos))
      datos
    }
  ))
  indice_catalogo <- match(secuencias$punto_canonico_id, puntos$canonical_point_id)
  if (anyNA(indice_catalogo)) {
    stop("Una secuencia referencia puntos ausentes del catalogo.", call. = FALSE)
  }
  secuencias$corredor_id <- puntos$corridor_id[indice_catalogo]
  secuencias$indice_punto_corredor <- puntos$corridor_point_index[indice_catalogo]
  secuencias$nombre_via <- puntos$osm_name[indice_catalogo]
  secuencias$zona_red <- puntos$network_zone[indice_catalogo]
  sf::st_geometry(secuencias) <- sf::st_geometry(puntos[indice_catalogo, ])

  calidad <- data.table::as.data.table(sf::st_drop_geometry(diagnostico))[, .(
    muestras = .N,
    proporcion_aceptada = mean(distancia_canonica_m <= distancia_aceptada_m),
    proporcion_cubierta = mean(distancia_canonica_m <= distancia_revision_m),
    distancia_mediana_m = stats::median(distancia_canonica_m),
    distancia_p95_m = as.numeric(stats::quantile(distancia_canonica_m, 0.95)),
    distancia_maxima_m = max(distancia_canonica_m),
    corredores_usados = data.table::uniqueN(corredor_id),
    asignaciones_suavizadas = sum(corredor_inicial_id != corredor_id)
  ), by = .(
    codigo_variante, id_version_variante, codigo_linea,
    nombre_linea, sentido, zona_red
  )]
  list(secuencias = secuencias, diagnostico = diagnostico, calidad = calidad)
}

asignar_paradas_variantes <- function(
    paradas,
    universo,
    secuencias,
    puntos,
    crs_geografico = 4326L,
    distancia_aceptada_m = 75,
    distancia_revision_m = 150,
    informar_progreso = FALSE,
    cada_variantes = 100L) {
  paradas <- data.table::copy(data.table::as.data.table(paradas))
  universo <- data.table::as.data.table(universo)
  paradas[, codigo_variante := as.character(cod_variante)]
  paradas[, ordinal := suppressWarnings(as.numeric(ordinal))]
  versiones <- universo[, .(codigo_variante, id_version_variante)]
  paradas <- versiones[paradas, on = "codigo_variante", nomatch = 0L]
  validas <- is.finite(paradas$x) & is.finite(paradas$y)
  if (!all(validas)) {
    stop("El universo contiene paradas sin coordenadas validas.", call. = FALSE)
  }
  paradas_sf <- sf::st_as_sf(
    as.data.frame(paradas), coords = c("x", "y"), crs = crs_geografico,
    remove = FALSE
  )
  paradas_sf <- sf::st_transform(paradas_sf, sf::st_crs(secuencias))
  paradas_sf$punto_espacial_id <- NA_character_
  paradas_sf$indice_secuencia_espacial <- NA_integer_
  paradas_sf$distancia_espacial_m <- NA_real_
  paradas_sf$punto_canonico_id <- NA_character_
  paradas_sf$indice_secuencia <- NA_integer_
  paradas_sf$metodo_asignacion <- NA_character_

  versiones_paradas <- unique(paradas_sf$id_version_variante)
  for (indice_version in seq_along(versiones_paradas)) {
    version <- versiones_paradas[[indice_version]]
    if (isTRUE(informar_progreso) &&
        (indice_version %% cada_variantes == 0L || indice_version == length(versiones_paradas))) {
      cat(sprintf(
        "[paradas] %s/%s variantes procesadas.\n",
        indice_version, length(versiones_paradas)
      ))
    }
    filas <- which(paradas_sf$id_version_variante == version)
    secuencia <- secuencias[secuencias$id_version_variante == version, ]
    secuencia <- secuencia[order(secuencia$indice_secuencia), ]
    if (!nrow(secuencia)) next
    cercanos <- sf::st_nearest_feature(paradas_sf[filas, ], secuencia)
    distancias <- as.numeric(sf::st_distance(
      paradas_sf[filas, ], secuencia[cercanos, ], by_element = TRUE
    ))
    paradas_sf$punto_espacial_id[filas] <- secuencia$punto_canonico_id[cercanos]
    paradas_sf$indice_secuencia_espacial[filas] <- secuencia$indice_secuencia[cercanos]
    paradas_sf$distancia_espacial_m[filas] <- distancias
    paradas_sf$punto_canonico_id[filas] <- secuencia$punto_canonico_id[cercanos]
    paradas_sf$indice_secuencia[filas] <- secuencia$indice_secuencia[cercanos]
    paradas_sf$metodo_asignacion[filas] <- ifelse(
      paradas_sf$ordinal[filas] > 0, "espacial", "espacial_sin_ordinal"
    )

    candidatas <- which(paradas_sf$ordinal[filas] > 0 & distancias > distancia_revision_m)
    anclas <- which(paradas_sf$ordinal[filas] > 0 & distancias <= distancia_revision_m)
    if (!length(candidatas)) next
    if (length(anclas) >= 2L &&
        data.table::uniqueN(paradas_sf$ordinal[filas][anclas]) >= 2L) {
      tabla_anclas <- data.table::data.table(
        ordinal = paradas_sf$ordinal[filas][anclas],
        indice = cercanos[anclas]
      )[, .(indice = as.numeric(stats::median(indice))), by = ordinal][order(ordinal)]
      predichos <- stats::approx(
        tabla_anclas$ordinal, tabla_anclas$indice,
        xout = paradas_sf$ordinal[filas][candidatas], rule = 2
      )$y
      metodo <- "interpolacion_ordinal"
    } else {
      ordinales <- paradas_sf$ordinal[filas]
      rango <- range(ordinales[ordinales > 0], na.rm = TRUE)
      predichos <- if (diff(rango) == 0) {
        rep((nrow(secuencia) + 1) / 2, length(candidatas))
      } else {
        1 + (paradas_sf$ordinal[filas][candidatas] - rango[[1]]) /
          diff(rango) * (nrow(secuencia) - 1)
      }
      metodo <- "rango_ordinal"
    }
    predichos <- pmax(1L, pmin(nrow(secuencia), as.integer(round(predichos))))
    filas_globales <- filas[candidatas]
    paradas_sf$punto_canonico_id[filas_globales] <- secuencia$punto_canonico_id[predichos]
    paradas_sf$indice_secuencia[filas_globales] <- secuencia$indice_secuencia[predichos]
    paradas_sf$metodo_asignacion[filas_globales] <- metodo
  }

  indice_punto <- match(paradas_sf$punto_canonico_id, puntos$canonical_point_id)
  if (anyNA(indice_punto)) stop("Alguna parada quedo sin punto canonico.", call. = FALSE)
  paradas_sf$corredor_id <- puntos$corridor_id[indice_punto]
  paradas_sf$distancia_asignada_m <- as.numeric(sf::st_distance(
    paradas_sf, puntos[indice_punto, ], by_element = TRUE
  ))
  paradas_sf$estado_calidad <- ifelse(
    paradas_sf$metodo_asignacion %in% c("interpolacion_ordinal", "rango_ordinal"),
    "retenida_por_ordinal",
    ifelse(
      paradas_sf$distancia_espacial_m <= distancia_aceptada_m, "aceptada",
      ifelse(paradas_sf$distancia_espacial_m <= distancia_revision_m, "revision", "atipica_retenida")
    )
  )
  calidad <- data.table::as.data.table(sf::st_drop_geometry(paradas_sf))[, .(
    paradas = .N,
    aceptadas = sum(estado_calidad == "aceptada"),
    revision = sum(estado_calidad == "revision"),
    retenidas_por_ordinal = sum(estado_calidad == "retenida_por_ordinal"),
    maxima_distancia_espacial_m = max(distancia_espacial_m),
    maxima_distancia_asignada_m = max(distancia_asignada_m)
  ), by = .(codigo_variante, id_version_variante)]
  list(paradas = paradas_sf, calidad = calidad)
}

validar_variantes_canonicas <- function(universo, secuencias, paradas, puntos) {
  secuencia <- data.table::as.data.table(sf::st_drop_geometry(secuencias))
  parada <- data.table::as.data.table(sf::st_drop_geometry(paradas))
  universo <- data.table::as.data.table(universo)
  claves_duplicadas <- anyDuplicated(secuencia[, .(id_version_variante, indice_secuencia)])
  continuidad <- secuencia[, identical(indice_secuencia, seq_len(.N)), by = id_version_variante]$V1
  controles <- data.table::data.table(
    control = c(
      "todas_las_variantes_tienen_secuencia", "posiciones_unicas",
      "indices_contiguos", "puntos_resuelven", "todas_las_paradas_asignadas",
      "versiones_paradas_resuelven"
    ),
    aprobado = c(
      setequal(universo$id_version_variante, unique(secuencia$id_version_variante)),
      claves_duplicadas == 0L,
      all(continuidad),
      all(secuencia$punto_canonico_id %in% puntos$canonical_point_id),
      !anyNA(parada$punto_canonico_id),
      all(parada$id_version_variante %in% universo$id_version_variante)
    )
  )
  controles[, estado := ifelse(aprobado, "aprobado", "fallo")]
  controles
}
