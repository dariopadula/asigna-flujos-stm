migrar_red_canonica_inicial <- function(
    version_red,
    archivo_corredores,
    archivo_puntos,
    directorio_registro,
    corredores_esperados = NULL,
    puntos_esperados = NULL,
    crs_esperado = 32721L) {
  for (paquete in c("data.table", "sf", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  entradas <- c(archivo_corredores, archivo_puntos)
  inexistentes <- entradas[!file.exists(entradas)]
  if (length(inexistentes)) {
    stop(sprintf("Faltan objetos de bootstrap: %s", paste(inexistentes, collapse = ", ")), call. = FALSE)
  }
  destino <- file.path(directorio_registro, paste0("red=", version_red))
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf("La red canonica ya existe y no sera sobrescrita: %s", destino), call. = FALSE)
  }

  corredores <- readRDS(archivo_corredores)
  puntos <- readRDS(archivo_puntos)
  if (!inherits(corredores, "sf") || !inherits(puntos, "sf")) {
    stop("Los objetos de bootstrap deben ser sf.", call. = FALSE)
  }
  requeridas_corredores <- c("corridor_id", "geometry")
  requeridas_puntos <- c("canonical_point_id", "corridor_id", "geometry")
  if (length(setdiff(requeridas_corredores, names(corredores)))) {
    stop("El catalogo de corredores no cumple el contrato minimo.", call. = FALSE)
  }
  if (length(setdiff(requeridas_puntos, names(puntos)))) {
    stop("El catalogo de puntos no cumple el contrato minimo.", call. = FALSE)
  }
  if (anyNA(corredores$corridor_id) || anyDuplicated(corredores$corridor_id)) {
    stop("corridor_id debe ser unico y no puede tener faltantes.", call. = FALSE)
  }
  if (anyNA(puntos$canonical_point_id) || anyDuplicated(puntos$canonical_point_id)) {
    stop("canonical_point_id debe ser unico y no puede tener faltantes.", call. = FALSE)
  }
  if (!all(puntos$corridor_id %in% corredores$corridor_id)) {
    stop("La red contiene puntos sin corredor registrado.", call. = FALSE)
  }
  if (is.na(sf::st_crs(corredores)$epsg) || sf::st_crs(corredores)$epsg != crs_esperado ||
      is.na(sf::st_crs(puntos)$epsg) || sf::st_crs(puntos)$epsg != crs_esperado) {
    stop(sprintf("La red debe usar EPSG:%s.", crs_esperado), call. = FALSE)
  }
  if (!is.null(corredores_esperados) && nrow(corredores) != as.integer(corredores_esperados)) {
    stop("La cantidad de corredores no coincide con el bootstrap esperado.", call. = FALSE)
  }
  if (!is.null(puntos_esperados) && nrow(puntos) != as.integer(puntos_esperados)) {
    stop("La cantidad de puntos no coincide con el bootstrap esperado.", call. = FALSE)
  }

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(dirname(destino), paste0(".tmp_", version_red, "_", Sys.getpid()))
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) unlink(temporal, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  destinos <- file.path(temporal, c("catalogo_corredores.rds", "catalogo_puntos.rds"))
  copiados <- file.copy(
    entradas, destinos, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE
  )
  if (!all(copiados) || any(!file.exists(destinos))) {
    stop("No se pudieron copiar los objetos de bootstrap.", call. = FALSE)
  }
  md5_origen <- unname(tools::md5sum(entradas))
  md5_destino <- unname(tools::md5sum(destinos))
  if (!identical(md5_origen, md5_destino)) {
    stop("Las huellas de los objetos migrados no coinciden con el origen.", call. = FALSE)
  }
  manifiesto_archivos <- data.table::data.table(
    objeto = c("corredores", "puntos"),
    archivo_origen = normalizePath(entradas, winslash = "/"),
    archivo_destino = basename(destinos),
    filas = c(nrow(corredores), nrow(puntos)),
    md5_origen,
    md5_destino,
    copia_identica = md5_origen == md5_destino
  )
  data.table::fwrite(manifiesto_archivos, file.path(temporal, "manifiesto_archivos.csv"))
  yaml::write_yaml(list(
    version_red = version_red,
    tipo = "bootstrap_migrado",
    politica = "IDs y geometrias se conservan literalmente; no se reconstruyen",
    corredores = nrow(corredores),
    puntos = nrow(puntos),
    epsg = as.integer(crs_esperado),
    puntos_huerfanos = 0L,
    ids_corredor_unicos = TRUE,
    ids_punto_unicos = TRUE,
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))
  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover la red migrada a: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  list(destino = destino, manifiesto_archivos = manifiesto_archivos)
}
