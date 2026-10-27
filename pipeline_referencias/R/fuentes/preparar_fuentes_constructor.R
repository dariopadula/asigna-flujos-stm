archivos_shapefile <- function(ruta_shp) {
  directorio <- dirname(ruta_shp)
  base <- tools::file_path_sans_ext(basename(ruta_shp))
  archivos <- list.files(
    directorio,
    pattern = paste0("^", base, "[.](shp|shx|dbf|prj|cpg|qpj)$"),
    ignore.case = TRUE,
    full.names = TRUE
  )
  if (!ruta_shp %in% archivos && file.exists(ruta_shp)) archivos <- c(ruta_shp, archivos)
  unique(archivos)
}

copiar_fuente_constructor <- function(origen, destino) {
  archivos <- if (tolower(tools::file_ext(origen)) == "shp") {
    archivos_shapefile(origen)
  } else {
    origen
  }
  if (!length(archivos) || any(!file.exists(archivos))) {
    stop(sprintf("Falta la fuente del constructor: %s", origen), call. = FALSE)
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (tolower(tools::file_ext(origen)) == "shp") {
    base_destino <- tools::file_path_sans_ext(basename(destino))
    destinos <- file.path(
      dirname(destino), paste0(base_destino, ".", tools::file_ext(archivos))
    )
  } else {
    destinos <- destino
  }
  if (any(file.exists(destinos))) {
    stop(sprintf("La fuente ya fue preparada en: %s", dirname(destino)), call. = FALSE)
  }
  copiados <- file.copy(archivos, destinos, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
  if (!all(copiados)) stop(sprintf("No se pudo copiar la fuente: %s", origen), call. = FALSE)
  data.table::data.table(
    archivo_origen = normalizePath(archivos, winslash = "/"),
    archivo_destino = normalizePath(destinos, winslash = "/"),
    bytes = file.info(destinos)$size,
    md5_origen = unname(tools::md5sum(archivos)),
    md5_destino = unname(tools::md5sum(destinos))
  )
}

preparar_fuentes_constructor <- function(origenes, destinos, directorio_manifesto) {
  for (paquete in c("data.table", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  if (!identical(sort(names(origenes)), sort(names(destinos)))) {
    stop("Origenes y destinos deben declarar las mismas fuentes.", call. = FALSE)
  }
  if (dir.exists(directorio_manifesto) || file.exists(directorio_manifesto)) {
    stop("El manifiesto de fuentes ya existe y no sera sobrescrito.", call. = FALSE)
  }
  partes <- lapply(names(origenes), function(nombre) {
    resultado <- copiar_fuente_constructor(origenes[[nombre]], destinos[[nombre]])
    resultado[, fuente := nombre]
    resultado
  })
  manifiesto <- data.table::rbindlist(partes, fill = TRUE)
  if (any(manifiesto$md5_origen != manifiesto$md5_destino)) {
    stop("Alguna fuente preparada no coincide con su origen.", call. = FALSE)
  }
  dir.create(directorio_manifesto, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(manifiesto, file.path(directorio_manifesto, "manifiesto_archivos.csv"))
  yaml::write_yaml(list(
    tipo = "fuentes_constructor_canonico",
    fuentes = unique(manifiesto$fuente),
    archivos = nrow(manifiesto),
    bytes = sum(manifiesto$bytes),
    copias_identicas = TRUE,
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(directorio_manifesto, "manifiesto.yml"))
  manifiesto
}

validar_fuentes_constructor <- function(rutas, crs_metrico = 32721L) {
  for (paquete in c("data.table", "sf")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  requeridas <- c("urban_streets", "historical_stops", "montevideo_boundary", "osm_gpkg")
  faltantes_nombres <- setdiff(requeridas, names(rutas))
  if (length(faltantes_nombres)) stop("Faltan rutas requeridas del constructor.", call. = FALSE)
  controles <- data.table::data.table(
    control = paste0("existe_", requeridas),
    aprobado = file.exists(unlist(rutas[requeridas])),
    detalle = unlist(rutas[requeridas])
  )
  if (any(!controles$aprobado)) return(controles)

  vias <- sf::st_read(rutas$urban_streets, quiet = TRUE, options = "ADJUST_TYPE=NO")
  limite <- sf::st_read(rutas$montevideo_boundary, quiet = TRUE)
  columnas_paradas <- names(data.table::fread(rutas$historical_stops, nrows = 0L))
  capas_osm <- sf::st_layers(rutas$osm_gpkg)$name
  controles <- data.table::rbindlist(list(controles, data.table::data.table(
    control = c(
      "columnas_vias", "columnas_paradas_historicas", "limite_con_geometria",
      "osm_contiene_lines", "crs_vias_compatible"
    ),
    aprobado = c(
      all(c("NOM_CALLE", "COD_NOMBRE") %in% names(vias)),
      all(c(
        "stm_paradas_por_variante_historico.cod_ubic_parada",
        "stm_paradas_por_variante_historico.cod_nombre_via_1",
        "stm_paradas_por_variante_historico.the_geom"
      ) %in% columnas_paradas),
      nrow(limite) > 0L && !any(sf::st_is_empty(limite)),
      "lines" %in% capas_osm,
      is.na(sf::st_crs(vias)$epsg) || sf::st_crs(vias)$epsg == as.integer(crs_metrico)
    ),
    detalle = c("contrato", "contrato", "geometria", "capa lines", paste0("EPSG:", crs_metrico))
  )), fill = TRUE)
  controles
}
