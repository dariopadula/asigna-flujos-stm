publicar_registro_presentacion <- function(
    directorio_registro, directorio_publicado) {
  for (paquete in c("data.table", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete),
        call. = FALSE)
    }
  }
  archivo_manifiesto <- file.path(directorio_registro, "manifiesto.yml")
  archivo_inventario <- file.path(
    directorio_registro, "manifiesto_archivos.csv"
  )
  if (!file.exists(archivo_manifiesto) || !file.exists(archivo_inventario)) {
    stop("El registro de presentacion no tiene manifiestos completos.",
      call. = FALSE)
  }
  manifiesto <- yaml::read_yaml(archivo_manifiesto)
  if (!identical(as.character(manifiesto$tipo), "registro_presentacion") ||
      as.integer(manifiesto$controles_fallidos) != 0L) {
    stop("El registro de presentacion no esta aprobado.", call. = FALSE)
  }
  inventario <- data.table::fread(archivo_inventario)
  requeridas <- c("archivo", "bytes", "md5")
  if (!all(requeridas %in% names(inventario)) ||
      anyDuplicated(inventario$archivo)) {
    stop("El inventario del registro de presentacion es invalido.",
      call. = FALSE)
  }
  fuentes_nucleo <- file.path(directorio_registro, inventario$archivo)
  if (any(!file.exists(fuentes_nucleo)) ||
      !identical(unname(tools::md5sum(fuentes_nucleo)), inventario$md5)) {
    stop("Los archivos del registro no coinciden con su inventario.",
      call. = FALSE)
  }
  fuentes <- c(
    fuentes_nucleo,
    archivo_manifiesto,
    archivo_inventario
  )
  red <- as.character(manifiesto$red_canonica)
  version <- as.character(manifiesto$version_presentacion)
  relativo <- file.path(
    "referencias", paste0("red=", red), paste0("presentacion=", version)
  )
  destino <- file.path(directorio_publicado, relativo)
  if (dir.exists(destino)) {
    publicados <- file.path(destino, basename(fuentes))
    if (any(!file.exists(publicados)) || !identical(
        unname(tools::md5sum(fuentes)), unname(tools::md5sum(publicados)))) {
      stop("La referencia de presentacion publicada esta incompleta o difiere.",
        call. = FALSE)
    }
    return(list(destino = destino, relativo = relativo, reutilizado = TRUE))
  }
  raiz_red <- dirname(destino)
  temporal <- file.path(
    raiz_red, paste0(".preparando_presentacion=", version)
  )
  if (dir.exists(temporal)) {
    stop("Hay una publicacion de presentacion incompleta.", call. = FALSE)
  }
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  destinos <- file.path(temporal, basename(fuentes))
  if (!all(file.copy(fuentes, destinos, overwrite = FALSE))) {
    stop("No se pudo copiar la referencia de presentacion.", call. = FALSE)
  }
  if (!identical(
      unname(tools::md5sum(fuentes)), unname(tools::md5sum(destinos)))) {
    stop("La copia publicada no conserva las huellas del registro.",
      call. = FALSE)
  }
  yaml::write_yaml(list(
    tipo = "referencia_presentacion_publicada",
    estado = "listo",
    version_presentacion = version,
    version_registro_variantes =
      as.character(manifiesto$version_registro_variantes),
    snapshot_referencias = as.character(manifiesto$snapshot_referencias),
    red_canonica = red,
    dependencia_od = FALSE,
    origen_registro = normalizePath(
      directorio_registro, winslash = "/", mustWork = TRUE
    ),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto_publicacion.yml"))
  if (!file.rename(temporal, destino)) {
    stop("No se pudo promover atomicamente la referencia de presentacion.",
      call. = FALSE)
  }
  list(destino = destino, relativo = relativo, reutilizado = FALSE)
}
