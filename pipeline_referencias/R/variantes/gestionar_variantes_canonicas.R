`%||%` <- function(valor, alternativa) {
  if (is.null(valor)) alternativa else valor
}

resolver_ruta_variantes <- function(raiz_proyecto, ruta) {
  normalizePath(file.path(raiz_proyecto, ruta), winslash = "/", mustWork = FALSE)
}

rutas_variantes_canonicas <- function(raiz_proyecto, configuracion) {
  entrega_id <- configuracion$reference_snapshot$id
  red_id <- configuracion$canonical_variants$canonical_network_version
  raiz_candidata <- file.path(
    resolver_ruta_variantes(
      raiz_proyecto,
      configuracion$canonical_variants$outputs$candidate_root
    ),
    paste0("red=", red_id),
    paste0("entrega=", entrega_id)
  )
  list(
    entrega = file.path(
      resolver_ruta_variantes(raiz_proyecto, configuracion$paths$intermediate_references),
      paste0("entrega=", entrega_id)
    ),
    red = file.path(
      resolver_ruta_variantes(raiz_proyecto, configuracion$paths$canonical_registry),
      paste0("red=", red_id)
    ),
    candidata = raiz_candidata,
    proyeccion = file.path(raiz_candidata, "01_proyeccion_secuencias"),
    paradas = file.path(raiz_candidata, "02_asignacion_paradas"),
    validacion = file.path(raiz_candidata, "03_validacion"),
    registro = file.path(
      resolver_ruta_variantes(
        raiz_proyecto,
        configuracion$canonical_variants$outputs$registry_root
      ),
      paste0("red=", red_id),
      paste0("entrega=", entrega_id)
    ),
    oraculo = resolver_ruta_variantes(
      raiz_proyecto,
      configuracion$canonical_variants$oracle$root
    )
  )
}

checkpoint_variantes_completo <- function(destino, etapa, archivos_requeridos) {
  if (!dir.exists(destino)) return(FALSE)
  archivo_manifiesto <- file.path(destino, "manifiesto.yml")
  if (!file.exists(archivo_manifiesto)) return(FALSE)
  manifiesto <- tryCatch(
    yaml::read_yaml(archivo_manifiesto),
    error = function(error) NULL
  )
  if (is.null(manifiesto) ||
      !identical(manifiesto$estado, "completo") ||
      !identical(manifiesto$etapa, etapa)) {
    return(FALSE)
  }
  rutas_requeridas <- file.path(destino, archivos_requeridos)
  all(file.exists(rutas_requeridas)) && all(file.info(rutas_requeridas)$size > 0)
}

exigir_checkpoint_variantes <- function(destino, etapa, archivos_requeridos) {
  if (!checkpoint_variantes_completo(destino, etapa, archivos_requeridos)) {
    stop(sprintf(
      "El checkpoint '%s' no existe o esta incompleto: %s",
      etapa, destino
    ), call. = FALSE)
  }
  invisible(destino)
}

escribir_checkpoint_variantes <- function(
    destino,
    etapa,
    archivos_requeridos,
    escribir,
    metadatos = list()) {
  if (dir.exists(destino)) {
    if (checkpoint_variantes_completo(destino, etapa, archivos_requeridos)) {
      cat(sprintf("[%s] Checkpoint completo reutilizado: %s\n", etapa, destino))
      return(invisible(list(destino = destino, reutilizado = TRUE)))
    }
    stop(sprintf(
      "Existe una salida incompleta que no sera sobrescrita: %s",
      destino
    ), call. = FALSE)
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(
    dirname(destino),
    sprintf(".tmp_%s_%s", basename(destino), Sys.getpid())
  )
  if (dir.exists(temporal)) {
    stop(sprintf("Ya existe el temporal de la etapa: %s", temporal), call. = FALSE)
  }
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) {
      unlink(temporal, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  escribir(temporal)
  faltantes <- archivos_requeridos[
    !file.exists(file.path(temporal, archivos_requeridos))
  ]
  if (length(faltantes)) {
    stop(sprintf(
      "La etapa '%s' no produjo: %s",
      etapa, paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }
  manifiesto <- utils::modifyList(list(
    etapa = etapa,
    estado = "completo",
    archivos = archivos_requeridos,
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), metadatos)
  yaml::write_yaml(manifiesto, file.path(temporal, "manifiesto.yml"))
  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover el checkpoint: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  invisible(list(destino = destino, reutilizado = FALSE))
}

informar_fase_variantes <- function(fase, inicio = NULL, objeto = NULL) {
  if (is.null(inicio)) {
    cat(sprintf("[%s] Inicio: %s\n", fase, format(Sys.time(), "%H:%M:%S")))
    return(invisible(Sys.time()))
  }
  segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  memoria <- if (is.null(objeto)) "" else sprintf(
    " | memoria aproximada: %s",
    format(utils::object.size(objeto), units = "auto")
  )
  cat(sprintf("[%s] Fin: %.1f segundos%s\n", fase, segundos, memoria))
  invisible(segundos)
}

tabla_sf_desde_coordenadas <- function(tabla, x, y, crs) {
  tabla <- as.data.frame(tabla)
  if (length(setdiff(c(x, y), names(tabla)))) {
    stop("El checkpoint no contiene las coordenadas requeridas.", call. = FALSE)
  }
  sf::st_as_sf(tabla, coords = c(x, y), crs = crs, remove = FALSE)
}

archivos_checkpoint_proyeccion <- function() {
  c("secuencias.parquet", "diagnostico_proyeccion.parquet", "calidad_variantes.csv")
}

archivos_checkpoint_paradas <- function() {
  c("paradas_proyectadas.parquet", "calidad_paradas.csv")
}

ejecutar_proyeccion_variantes_canonicas <- function(raiz_proyecto, configuracion) {
  rutas <- rutas_variantes_canonicas(raiz_proyecto, configuracion)
  requeridos <- archivos_checkpoint_proyeccion()
  if (checkpoint_variantes_completo(rutas$proyeccion, "proyeccion_secuencias", requeridos)) {
    cat(sprintf("[proyeccion_secuencias] Checkpoint completo reutilizado: %s\n", rutas$proyeccion))
    return(invisible(rutas$proyeccion))
  }
  if (dir.exists(rutas$proyeccion)) {
    stop(sprintf("Existe un checkpoint de proyeccion incompleto: %s", rutas$proyeccion), call. = FALSE)
  }

  archivo_universo <- file.path(rutas$entrega, "universo_variantes.parquet")
  archivos_red <- file.path(rutas$red, c("catalogo_corredores.rds", "catalogo_puntos.rds"))
  limite_archivo <- resolver_ruta_variantes(
    raiz_proyecto,
    configuracion$canonical_constructor$sources$montevideo_boundary
  )
  faltantes <- c(archivo_universo, archivos_red, limite_archivo)[
    !file.exists(c(archivo_universo, archivos_red, limite_archivo))
  ]
  if (length(faltantes)) {
    stop(sprintf("Faltan entradas: %s", paste(faltantes, collapse = ", ")), call. = FALSE)
  }

  corredores <- readRDS(archivos_red[[1]])
  puntos <- readRDS(archivos_red[[2]])
  inicio_muestreo <- informar_fase_variantes("muestreo")
  muestras <- muestrear_universo_variantes(
    archivo_universo,
    configuracion$canonical_variants$projection$sample_distance_m,
    configuracion$reference_snapshot$metric_crs
  )
  informar_fase_variantes("muestreo", inicio_muestreo, muestras)
  limite <- cargar_limite_montevideo(
    limite_archivo,
    configuracion$reference_snapshot$metric_crs
  )
  inicio_proyeccion <- informar_fase_variantes("proyeccion_secuencias")
  proyeccion <- construir_secuencias_variantes(
    muestras, limite, corredores, puntos,
    configuracion$canonical_variants$projection$smooth_short_runs_max,
    configuracion$canonical_variants$projection$accept_distance_m,
    configuracion$canonical_variants$projection$review_distance_m,
    informar_progreso = TRUE,
    cada_variantes = configuracion$canonical_variants$progress_every %||% 100L
  )
  duracion <- informar_fase_variantes(
    "proyeccion_secuencias", inicio_proyeccion, proyeccion
  )

  tabla_secuencias <- data.table::as.data.table(sf::st_drop_geometry(proyeccion$secuencias))
  coordenadas_secuencia <- sf::st_coordinates(proyeccion$secuencias)
  tabla_secuencias[, `:=`(
    x_canonica = coordenadas_secuencia[, 1],
    y_canonica = coordenadas_secuencia[, 2]
  )]
  tabla_diagnostico <- data.table::as.data.table(sf::st_drop_geometry(proyeccion$diagnostico))
  coordenadas_diagnostico <- sf::st_coordinates(proyeccion$diagnostico)
  tabla_diagnostico[, `:=`(
    x_muestra = coordenadas_diagnostico[, 1],
    y_muestra = coordenadas_diagnostico[, 2]
  )]

  escribir_checkpoint_variantes(
    rutas$proyeccion,
    "proyeccion_secuencias",
    requeridos,
    function(temporal) {
      arrow::write_parquet(
        tabla_secuencias, file.path(temporal, "secuencias.parquet"), compression = "zstd"
      )
      arrow::write_parquet(
        tabla_diagnostico,
        file.path(temporal, "diagnostico_proyeccion.parquet"),
        compression = "zstd"
      )
      data.table::fwrite(proyeccion$calidad, file.path(temporal, "calidad_variantes.csv"))
    },
    list(
      version = configuracion$canonical_variants$version,
      snapshot_referencias = configuracion$reference_snapshot$id,
      red_canonica = configuracion$canonical_variants$canonical_network_version,
      dependencia_od = FALSE,
      variantes = data.table::uniqueN(tabla_secuencias$id_version_variante),
      posiciones_secuencia = nrow(tabla_secuencias),
      muestras = nrow(tabla_diagnostico),
      duracion_proyeccion_segundos = duracion
    )
  )
  cat(sprintf("Checkpoint de proyeccion terminado: %s\n", rutas$proyeccion))
  invisible(rutas$proyeccion)
}

leer_secuencias_checkpoint <- function(ruta, crs_metrico) {
  exigir_checkpoint_variantes(
    ruta, "proyeccion_secuencias", archivos_checkpoint_proyeccion()
  )
  tabla <- data.table::as.data.table(arrow::read_parquet(
    file.path(ruta, "secuencias.parquet")
  ))
  tabla_sf_desde_coordenadas(tabla, "x_canonica", "y_canonica", crs_metrico)
}

ejecutar_asignacion_paradas_canonicas <- function(raiz_proyecto, configuracion) {
  rutas <- rutas_variantes_canonicas(raiz_proyecto, configuracion)
  requeridos <- archivos_checkpoint_paradas()
  if (checkpoint_variantes_completo(rutas$paradas, "asignacion_paradas", requeridos)) {
    cat(sprintf("[asignacion_paradas] Checkpoint completo reutilizado: %s\n", rutas$paradas))
    return(invisible(rutas$paradas))
  }
  if (dir.exists(rutas$paradas)) {
    stop(sprintf("Existe un checkpoint de paradas incompleto: %s", rutas$paradas), call. = FALSE)
  }
  exigir_checkpoint_variantes(
    rutas$proyeccion, "proyeccion_secuencias", archivos_checkpoint_proyeccion()
  )
  archivos_entrada <- c(
    file.path(rutas$entrega, "universo_variantes.parquet"),
    file.path(rutas$entrega, "paradas_universo.parquet"),
    file.path(rutas$red, "catalogo_puntos.rds")
  )
  faltantes <- archivos_entrada[!file.exists(archivos_entrada)]
  if (length(faltantes)) {
    stop(sprintf("Faltan entradas: %s", paste(faltantes, collapse = ", ")), call. = FALSE)
  }
  universo <- data.table::as.data.table(arrow::read_parquet(archivos_entrada[[1]]))
  paradas <- data.table::as.data.table(arrow::read_parquet(archivos_entrada[[2]]))
  puntos <- readRDS(archivos_entrada[[3]])
  secuencias <- leer_secuencias_checkpoint(
    rutas$proyeccion, configuracion$reference_snapshot$metric_crs
  )
  inicio <- informar_fase_variantes("asignacion_paradas")
  asignacion <- asignar_paradas_variantes(
    paradas, universo, secuencias, puntos,
    4326L,
    configuracion$canonical_variants$stops$accept_distance_m,
    configuracion$canonical_variants$stops$review_distance_m,
    informar_progreso = TRUE,
    cada_variantes = configuracion$canonical_variants$progress_every %||% 100L
  )
  duracion <- informar_fase_variantes("asignacion_paradas", inicio, asignacion)
  tabla_paradas <- data.table::as.data.table(sf::st_drop_geometry(asignacion$paradas))
  escribir_checkpoint_variantes(
    rutas$paradas,
    "asignacion_paradas",
    requeridos,
    function(temporal) {
      arrow::write_parquet(
        tabla_paradas,
        file.path(temporal, "paradas_proyectadas.parquet"),
        compression = "zstd"
      )
      data.table::fwrite(asignacion$calidad, file.path(temporal, "calidad_paradas.csv"))
    },
    list(
      version = configuracion$canonical_variants$version,
      snapshot_referencias = configuracion$reference_snapshot$id,
      red_canonica = configuracion$canonical_variants$canonical_network_version,
      variantes = data.table::uniqueN(tabla_paradas$id_version_variante),
      paradas = nrow(tabla_paradas),
      duracion_asignacion_segundos = duracion
    )
  )
  cat(sprintf("Checkpoint de paradas terminado: %s\n", rutas$paradas))
  invisible(rutas$paradas)
}

comparar_secuencias_oraculo <- function(secuencias, archivo_oraculo) {
  if (!file.exists(archivo_oraculo)) {
    stop(sprintf("No existe el oraculo de secuencias: %s", archivo_oraculo), call. = FALSE)
  }
  actual <- data.table::as.data.table(secuencias)[, .(
    variant_version_id = as.character(id_version_variante),
    sequence_index = as.integer(indice_secuencia),
    canonical_point_id = as.character(punto_canonico_id)
  )]
  oraculo <- data.table::fread(
    archivo_oraculo,
    select = c("variant_version_id", "sequence_index", "canonical_point_id")
  )
  oraculo[, `:=`(
    variant_version_id = as.character(variant_version_id),
    sequence_index = as.integer(sequence_index),
    canonical_point_id = as.character(canonical_point_id)
  )]
  actual <- unique(actual)
  oraculo <- unique(oraculo)
  faltantes <- data.table::fsetdiff(oraculo, actual)
  adicionales <- data.table::fsetdiff(actual, oraculo)
  list(
    resumen = data.table::data.table(
      metrica = c(
        "claves_oraculo", "claves_actuales", "faltantes_en_actual",
        "adicionales_en_actual", "igualdad_exacta"
      ),
      valor = c(
        nrow(oraculo), nrow(actual), nrow(faltantes), nrow(adicionales),
        as.integer(!nrow(faltantes) && !nrow(adicionales))
      )
    ),
    faltantes = faltantes,
    adicionales = adicionales,
    igualdad_exacta = !nrow(faltantes) && !nrow(adicionales)
  )
}

resumir_calidad_proyeccion <- function(tabla, origen) {
  tabla <- data.table::as.data.table(tabla)
  tabla[, origen_datos := origen]
  tabla[, .(
    registros = .N,
    distancia_mediana_m = stats::median(distancia_m, na.rm = TRUE),
    distancia_p95_m = as.numeric(stats::quantile(distancia_m, 0.95, na.rm = TRUE)),
    distancia_maxima_m = max(distancia_m, na.rm = TRUE)
  ), by = .(origen = origen_datos, estado)]
}

ejecutar_validacion_promocion_variantes <- function(raiz_proyecto, configuracion) {
  rutas <- rutas_variantes_canonicas(raiz_proyecto, configuracion)
  exigir_checkpoint_variantes(
    rutas$proyeccion, "proyeccion_secuencias", archivos_checkpoint_proyeccion()
  )
  exigir_checkpoint_variantes(
    rutas$paradas, "asignacion_paradas", archivos_checkpoint_paradas()
  )
  archivo_universo <- file.path(rutas$entrega, "universo_variantes.parquet")
  archivo_puntos <- file.path(rutas$red, "catalogo_puntos.rds")
  archivo_oraculo <- file.path(rutas$oraculo, "secuencias_variantes.csv")
  universo <- data.table::as.data.table(arrow::read_parquet(archivo_universo))
  puntos <- readRDS(archivo_puntos)
  secuencias_sf <- leer_secuencias_checkpoint(
    rutas$proyeccion, configuracion$reference_snapshot$metric_crs
  )
  secuencias <- data.table::as.data.table(sf::st_drop_geometry(secuencias_sf))
  tabla_paradas <- data.table::as.data.table(arrow::read_parquet(
    file.path(rutas$paradas, "paradas_proyectadas.parquet")
  ))
  paradas_sf <- tabla_sf_desde_coordenadas(tabla_paradas, "x", "y", 4326L)
  controles_base <- validar_variantes_canonicas(
    universo, secuencias_sf, paradas_sf, puntos
  )
  comparacion <- comparar_secuencias_oraculo(secuencias, archivo_oraculo)
  esperadas <- configuracion$expected_initial
  controles_esperados <- data.table::data.table(
    control = c(
      "cantidad_variantes_esperada", "cantidad_posiciones_esperada",
      "cantidad_paradas_esperada", "claves_iguales_al_oraculo"
    ),
    aprobado = c(
      data.table::uniqueN(secuencias$id_version_variante) == esperadas$buildable_variants,
      nrow(secuencias) == configuracion$canonical_variants$oracle$expected_sequence_positions,
      nrow(tabla_paradas) == esperadas$universe_stops,
      comparacion$igualdad_exacta
    )
  )
  controles_esperados[, estado := ifelse(aprobado, "aprobado", "fallo")]
  controles <- data.table::rbindlist(list(controles_base, controles_esperados), fill = TRUE)
  controles[, severidad := "critica"]

  calidad_actual <- resumir_calidad_proyeccion(
    secuencias[, .(
      estado = estado_coincidencia,
      distancia_m = distancia_canonica_m
    )],
    "actual"
  )
  oraculo_calidad <- data.table::fread(
    archivo_oraculo,
    select = c("match_status", "canonical_distance_m")
  )
  calidad_oraculo <- resumir_calidad_proyeccion(
    oraculo_calidad[, .(
      estado = match_status,
      distancia_m = canonical_distance_m
    )],
    "oraculo"
  )
  resumen_calidad <- data.table::rbindlist(
    list(calidad_actual, calidad_oraculo), fill = TRUE
  )

  archivos_validacion <- c(
    "controles.csv", "comparacion_oraculo.csv", "claves_faltantes.csv",
    "claves_adicionales.csv", "resumen_calidad_proyeccion.csv"
  )
  escribir_checkpoint_variantes(
    rutas$validacion,
    "validacion",
    archivos_validacion,
    function(temporal) {
      data.table::fwrite(controles, file.path(temporal, "controles.csv"))
      data.table::fwrite(
        comparacion$resumen, file.path(temporal, "comparacion_oraculo.csv")
      )
      data.table::fwrite(
        comparacion$faltantes, file.path(temporal, "claves_faltantes.csv")
      )
      data.table::fwrite(
        comparacion$adicionales, file.path(temporal, "claves_adicionales.csv")
      )
      data.table::fwrite(
        resumen_calidad, file.path(temporal, "resumen_calidad_proyeccion.csv")
      )
    },
    list(
      version = configuracion$canonical_variants$version,
      controles_aprobados = sum(controles$aprobado),
      controles_fallidos = sum(!controles$aprobado),
      promocion_habilitada = all(controles$aprobado)
    )
  )
  if (any(!controles$aprobado)) {
    stop(sprintf(
      "La validacion fallo. Revise el checkpoint y no promueva: %s",
      rutas$validacion
    ), call. = FALSE)
  }

  archivos_registro <- c(
    "variantes.parquet", "secuencias.parquet", "paradas_proyectadas.parquet",
    "diagnostico_proyeccion.parquet", "calidad_variantes.csv",
    "calidad_paradas.csv", "controles.csv", "comparacion_oraculo.csv",
    "resumen_calidad_proyeccion.csv"
  )
  fuentes <- c(
    archivo_universo,
    file.path(rutas$proyeccion, "secuencias.parquet"),
    file.path(rutas$paradas, "paradas_proyectadas.parquet"),
    file.path(rutas$proyeccion, "diagnostico_proyeccion.parquet"),
    file.path(rutas$proyeccion, "calidad_variantes.csv"),
    file.path(rutas$paradas, "calidad_paradas.csv"),
    file.path(rutas$validacion, "controles.csv"),
    file.path(rutas$validacion, "comparacion_oraculo.csv"),
    file.path(rutas$validacion, "resumen_calidad_proyeccion.csv")
  )
  escribir_checkpoint_variantes(
    rutas$registro,
    "registro_canonico",
    archivos_registro,
    function(temporal) {
      copiados <- file.copy(fuentes, file.path(temporal, archivos_registro))
      if (!all(copiados)) stop("No se pudieron copiar todos los artefactos al registro.", call. = FALSE)
    },
    list(
      version = configuracion$canonical_variants$version,
      snapshot_referencias = configuracion$reference_snapshot$id,
      red_canonica = configuracion$canonical_variants$canonical_network_version,
      dependencia_od = FALSE,
      variantes = data.table::uniqueN(secuencias$id_version_variante),
      posiciones_secuencia = nrow(secuencias),
      paradas = nrow(tabla_paradas),
      igualdad_exacta_oraculo = comparacion$igualdad_exacta
    )
  )
  cat(sprintf("Registro canonico promovido: %s\n", rutas$registro))
  invisible(rutas$registro)
}
