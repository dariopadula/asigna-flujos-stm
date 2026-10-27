normalizar_nombre_region <- function(x) {
  valor <- toupper(trimws(as.character(x)))
  valor <- iconv(valor, from = "", to = "ASCII//TRANSLIT")
  valor[is.na(x) | !nzchar(valor)] <- NA_character_
  valor
}

leer_capa_regional <- function(archivo, crs_asumido = NULL) {
  capa <- sf::st_read(archivo, quiet = TRUE, stringsAsFactors = FALSE)
  if (is.na(sf::st_crs(capa))) {
    if (is.null(crs_asumido)) {
      stop(sprintf("La capa no declara CRS: %s", archivo), call. = FALSE)
    }
    sf::st_crs(capa) <- crs_asumido
  }
  sf::st_make_valid(capa)
}

asignar_region_a_paradas <- function(paradas, capa, columna) {
  capa <- sf::st_transform(capa, sf::st_crs(paradas))
  intersecciones <- sf::st_intersects(paradas, capa)
  valores <- as.character(capa[[columna]])
  asignados <- vapply(intersecciones, function(indices) {
    if (!length(indices)) return(NA_character_)
    sort(unique(valores[indices]))[[1L]]
  }, character(1L))
  list(
    valores = asignados,
    ambiguas = sum(lengths(intersecciones) > 1L)
  )
}

construir_registro_regiones <- function(
    version_regiones,
    archivo_paradas_canonicas,
    archivo_barrios,
    archivo_ccz,
    archivo_municipios,
    archivo_departamentos,
    directorio_registro,
    version_variantes,
    snapshot_referencias,
    red_canonica,
    departamentos_objetivo = c("MONTEVIDEO", "CANELONES", "SAN JOSE"),
    crs_departamentos = 32721L) {
  for (paquete in c("arrow", "data.table", "sf", "tidyselect", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete),
        call. = FALSE)
    }
  }
  entradas <- c(
    archivo_paradas_canonicas, archivo_barrios, archivo_ccz,
    archivo_municipios, archivo_departamentos
  )
  faltantes <- entradas[!file.exists(entradas)]
  if (length(faltantes)) {
    stop(sprintf(
      "Faltan fuentes para el registro regional: %s",
      paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }
  destino <- file.path(
    directorio_registro, paste0("version=", version_regiones)
  )
  temporal <- file.path(
    directorio_registro, paste0(".preparando_version=", version_regiones)
  )
  if (dir.exists(destino) || dir.exists(temporal)) {
    stop(sprintf(
      "El registro regional ya existe o tiene una preparacion pendiente: %s",
      version_regiones
    ), call. = FALSE)
  }
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)

  paradas <- data.table::as.data.table(arrow::read_parquet(
    archivo_paradas_canonicas,
    col_select = tidyselect::all_of(c(
      "cod_ubic_parada", "desc_ubic_parada", "x", "y"
    ))
  ))
  paradas[, `:=`(
    codigo_parada = trimws(as.character(cod_ubic_parada)),
    nombre_parada = as.character(desc_ubic_parada),
    longitud = as.numeric(x),
    latitud = as.numeric(y)
  )]
  paradas[, c("cod_ubic_parada", "desc_ubic_parada", "x", "y") := NULL]
  paradas <- unique(paradas)
  ambiguas <- paradas[, .(
    combinaciones = data.table::uniqueN(paste(
      nombre_parada, longitud, latitud, sep = "|"
    ))
  ), by = codigo_parada][combinaciones != 1L]
  if (nrow(ambiguas) || anyNA(paradas) ||
      any(!is.finite(paradas$longitud)) ||
      any(!is.finite(paradas$latitud))) {
    stop("El maestro de paradas no define una geometria unica por codigo.",
      call. = FALSE)
  }
  paradas <- unique(paradas, by = "codigo_parada")
  puntos <- sf::st_as_sf(
    as.data.frame(paradas),
    coords = c("longitud", "latitud"), crs = 4326, remove = FALSE
  )

  barrios <- leer_capa_regional(archivo_barrios)
  if (!"NOMBBARR" %in% names(barrios)) {
    stop("La capa de barrios no contiene NOMBBARR.", call. = FALSE)
  }
  barrios$barrio <- normalizar_nombre_region(barrios$NOMBBARR)
  barrios <- barrios[, "barrio"]

  ccz <- leer_capa_regional(archivo_ccz)
  if (!"ZONA_LEGAL" %in% names(ccz)) {
    stop("La capa de CCZ no contiene ZONA_LEGAL.", call. = FALSE)
  }
  ccz$ccz <- normalizar_nombre_region(ccz$ZONA_LEGAL)
  ccz <- ccz[, "ccz"]

  municipios <- leer_capa_regional(archivo_municipios)
  if (!"MUNICIPIO" %in% names(municipios)) {
    stop("La capa de municipios no contiene MUNICIPIO.", call. = FALSE)
  }
  municipios$municipio <- normalizar_nombre_region(municipios$MUNICIPIO)
  municipios <- municipios[, "municipio"]

  departamentos <- leer_capa_regional(
    archivo_departamentos, crs_asumido = crs_departamentos
  )
  if (!"NOMBRE" %in% names(departamentos)) {
    stop("La capa de departamentos no contiene NOMBRE.", call. = FALSE)
  }
  departamentos$departamento <- normalizar_nombre_region(
    departamentos$NOMBRE
  )
  departamentos_objetivo <- normalizar_nombre_region(
    departamentos_objetivo
  )
  departamentos <- departamentos[
    departamentos$departamento %in% departamentos_objetivo,
    "departamento"
  ]
  if (!setequal(departamentos$departamento, departamentos_objetivo)) {
    stop("No estan disponibles todos los departamentos objetivo.",
      call. = FALSE)
  }

  resultado_barrio <- asignar_region_a_paradas(puntos, barrios, "barrio")
  resultado_ccz <- asignar_region_a_paradas(puntos, ccz, "ccz")
  resultado_municipio <- asignar_region_a_paradas(
    puntos, municipios, "municipio"
  )
  resultado_departamento <- asignar_region_a_paradas(
    puntos, departamentos, "departamento"
  )
  regiones <- data.table::copy(paradas)
  regiones[, `:=`(
    barrio = resultado_barrio$valores,
    ccz = resultado_ccz$valores,
    municipio = resultado_municipio$valores,
    departamento = resultado_departamento$valores
  )]

  union_municipios <- sf::st_union(sf::st_geometry(sf::st_transform(
    municipios, 4326
  )))
  dentro_montevideo <- lengths(sf::st_intersects(puntos, union_municipios)) > 0L
  resumen <- data.table::data.table(
    indicador = c(
      "paradas_total", "paradas_montevideo", "paradas_exteriores",
      "montevideo_sin_barrio", "montevideo_sin_ccz",
      "montevideo_sin_municipio", "paradas_departamentos_objetivo",
      "paradas_fuera_departamentos_objetivo", "intersecciones_ambiguas_barrio",
      "intersecciones_ambiguas_ccz", "intersecciones_ambiguas_municipio",
      "intersecciones_ambiguas_departamento"
    ),
    valor = c(
      nrow(regiones), sum(dentro_montevideo), sum(!dentro_montevideo),
      sum(dentro_montevideo & is.na(regiones$barrio)),
      sum(dentro_montevideo & is.na(regiones$ccz)),
      sum(dentro_montevideo & is.na(regiones$municipio)),
      sum(!is.na(regiones$departamento)),
      sum(is.na(regiones$departamento)),
      resultado_barrio$ambiguas, resultado_ccz$ambiguas,
      resultado_municipio$ambiguas, resultado_departamento$ambiguas
    )
  )
  controles <- data.table::data.table(
    control = c(
      "paradas_unicas", "coordenadas_completas", "barrios_disponibles",
      "ccz_disponibles", "municipios_disponibles",
      "departamentos_objetivo_disponibles",
      "montevideo_con_barrio", "montevideo_con_ccz",
      "montevideo_con_municipio", "departamento_montevideo_coincide",
      "salida_conserva_paradas", "salida_sin_region_vacia"
    ),
    aprobado = c(
      !anyDuplicated(regiones$codigo_parada),
      !anyNA(regiones[, .(codigo_parada, longitud, latitud)]),
      nrow(barrios) > 0L,
      nrow(ccz) > 0L,
      nrow(municipios) > 0L,
      setequal(departamentos$departamento, departamentos_objetivo),
      !any(dentro_montevideo & is.na(regiones$barrio)),
      !any(dentro_montevideo & is.na(regiones$ccz)),
      !any(dentro_montevideo & is.na(regiones$municipio)),
      !anyNA(regiones$departamento[dentro_montevideo]) &&
        all(regiones$departamento[dentro_montevideo] == "MONTEVIDEO"),
      nrow(regiones) == data.table::uniqueN(paradas$codigo_parada),
      any(!is.na(regiones$departamento))
    )
  )
  controles[, estado := data.table::fifelse(
    aprobado, "aprobado", "fallo"
  )]
  if (any(!controles$aprobado)) {
    stop("El registro regional no aprobo sus controles.", call. = FALSE)
  }

  formas <- list(
    barrios = sf::st_transform(barrios, 4326),
    ccz = sf::st_transform(ccz, 4326),
    municipios = sf::st_transform(municipios, 4326),
    departamentos = sf::st_transform(departamentos, 4326)
  )
  arrow::write_parquet(
    regiones, file.path(temporal, "paradas_regiones.parquet"),
    compression = "zstd"
  )
  saveRDS(formas, file.path(temporal, "formas_regiones.rds"),
    compress = FALSE)
  data.table::fwrite(resumen, file.path(temporal, "resumen_regiones.csv"))
  data.table::fwrite(controles, file.path(temporal, "controles.csv"))
  archivos <- list.files(temporal, full.names = TRUE)
  inventario <- data.table::data.table(
    archivo = basename(archivos),
    bytes = as.numeric(file.info(archivos)$size),
    md5 = unname(tools::md5sum(archivos))
  )
  data.table::fwrite(
    inventario, file.path(temporal, "manifiesto_archivos.csv")
  )
  yaml::write_yaml(list(
    tipo = "registro_regiones",
    version_regiones = version_regiones,
    version_registro_variantes = version_variantes,
    snapshot_referencias = snapshot_referencias,
    red_canonica = red_canonica,
    crs_paradas = 4326L,
    departamentos_objetivo = as.list(departamentos_objetivo),
    paradas = nrow(regiones),
    paradas_departamentos_objetivo = sum(!is.na(regiones$departamento)),
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))
  dir.create(directorio_registro, recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(temporal, destino)) {
    stop("No se pudo promover atomicamente el registro regional.",
      call. = FALSE)
  }
  list(
    destino = destino,
    regiones = regiones,
    resumen = resumen,
    controles = controles
  )
}
