normalizar_nombre_via <- function(x) {
  z <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  z <- gsub("^_+|_+$", "", gsub("[^A-Z0-9]+", "_", toupper(trimws(z))))
  z[is.na(z) | !nzchar(z)] <- NA_character_
  z
}

identidad_via <- function(corredor, nombre) {
  z <- normalizar_nombre_via(nombre)
  data.table::fifelse(!is.na(z), paste0("NAME::", z),
    paste0("CORRIDOR::", corredor))
}

sentido_inverso <- function(x) {
  unname(c(N_S = "S_N", S_N = "N_S", E_O = "O_E", O_E = "E_O")[x])
}

abreviar_sentido <- function(x) {
  unname(c(NORTE_SUR = "N_S", SUR_NORTE = "S_N",
    ESTE_OESTE = "E_O", OESTE_ESTE = "O_E")[x])
}

construir_orientacion_corredores <- function(
    puntos, vias_sin_sentido = character(), referencia_urbana = NULL) {
  tabla <- data.table::as.data.table(sf::st_drop_geometry(puntos))
  xy <- sf::st_coordinates(puntos)
  tabla[, `:=`(x__ = xy[, 1], y__ = xy[, 2])]
  data.table::setorder(tabla, corridor_id, corridor_point_index)
  resultado <- tabla[, {
    vx <- if (.N > 1L) stats::var(x__) else NA_real_
    vy <- if (.N > 1L) stats::var(y__) else NA_real_
    eje <- if (!is.finite(vx) || !is.finite(vy) || vx + vy == 0) {
      "UNKNOWN"
    } else if (vx < vy) "NS" else "EO"
    delta <- if (eje == "NS") y__[.N] - y__[1L] else x__[.N] - x__[1L]
    if (is.finite(delta) && delta == 0 && eje == "NS") {
      delta <- which.max(y__) - which.min(y__)
    } else if (is.finite(delta) && delta == 0 && eje == "EO") {
      delta <- which.max(x__) - which.min(x__)
    }
    adelante <- if (eje == "NS" && is.finite(delta) && delta != 0) {
      if (delta < 0) "N_S" else "S_N"
    } else if (eje == "EO" && is.finite(delta) && delta != 0) {
      if (delta < 0) "E_O" else "O_E"
    } else "INDETERMINADO"
    list(
      eje_orientacion = eje,
      sentido_adelante = adelante,
      sentido_atras = if (adelante %in% c("N_S", "S_N", "E_O", "O_E"))
        sentido_inverso(adelante) else "INDETERMINADO",
      confianza_orientacion = if (is.finite(vx) && is.finite(vy) && vx + vy > 0)
        abs(vx - vy) / (vx + vy) else NA_real_,
      fuente_orientacion = "varianza_catalogo",
      puntos_canonicos = .N
    )
  }, by = .(corredor_id = corridor_id, zona_red = network_zone)]

  if (!is.null(referencia_urbana)) {
    ref <- data.table::as.data.table(sf::st_drop_geometry(referencia_urbana))
    faltantes <- setdiff(c("COD_NOMBRE", "dirIni", "dirFin"), names(ref))
    if (length(faltantes)) stop(sprintf(
      "La referencia urbana no contiene: %s.", paste(faltantes, collapse = ", ")
    ), call. = FALSE)
    ref <- unique(ref[, .(
      corredor_id = paste0("MVD_", COD_NOMBRE),
      sentido_ref = abreviar_sentido(paste(dirIni, dirFin, sep = "_"))
    )], by = "corredor_id")
    resultado[ref, on = "corredor_id", `:=`(
      sentido_adelante = data.table::fifelse(
        !is.na(i.sentido_ref), i.sentido_ref, sentido_adelante),
      eje_orientacion = data.table::fcase(
        i.sentido_ref %in% c("N_S", "S_N"), "NS",
        i.sentido_ref %in% c("E_O", "O_E"), "EO",
        default = eje_orientacion),
      fuente_orientacion = data.table::fifelse(
        !is.na(i.sentido_ref), "referencia_canonica_urbana",
        fuente_orientacion)
    )]
    resultado[, sentido_atras := data.table::fifelse(
      sentido_adelante %in% c("N_S", "S_N", "E_O", "O_E"),
      sentido_inverso(sentido_adelante), "INDETERMINADO")]
  }
  codigo <- sub("^MVD_", "", resultado$corredor_id)
  resultado[, sentido_aplicable := !(
    corredor_id %in% as.character(vias_sin_sentido) |
      codigo %in% as.character(vias_sin_sentido))]
  resultado[sentido_aplicable == FALSE, `:=`(
    eje_orientacion = "NONE", sentido_adelante = "SIN_SENTIDO",
    sentido_atras = "SIN_SENTIDO", fuente_orientacion = "configurada_sin_sentido")]
  resultado[]
}

preparar_secuencia_presentacion <- function(secuencias, puntos) {
  req_s <- c("codigo_variante", "id_version_variante", "codigo_linea",
    "nombre_linea", "sentido", "indice_secuencia", "punto_canonico_id",
    "corredor_id", "indice_punto_corredor", "x_canonica", "y_canonica")
  req_p <- c("canonical_point_id", "corridor_id", "corridor_point_index",
    "osm_name", "network_zone")
  fs <- setdiff(req_s, names(secuencias)); fp <- setdiff(req_p, names(puntos))
  if (length(fs) || length(fp)) stop(sprintf(
    "Faltan columnas: secuencia=[%s]; catalogo=[%s].",
    paste(fs, collapse = ", "), paste(fp, collapse = ", ")
  ), call. = FALSE)
  catalogo <- data.table::as.data.table(sf::st_drop_geometry(puntos))
  if (anyDuplicated(catalogo$canonical_point_id))
    stop("El catalogo contiene puntos canonicos duplicados.", call. = FALSE)
  i <- match(as.character(secuencias$punto_canonico_id),
    as.character(catalogo$canonical_point_id))
  if (anyNA(i)) stop("Hay puntos de secuencia ausentes en el catalogo.",
    call. = FALSE)
  cp <- catalogo[i]
  if (any(as.character(secuencias$corredor_id) != as.character(cp$corridor_id)) ||
      any(as.integer(secuencias$indice_punto_corredor) !=
        as.integer(cp$corridor_point_index)))
    stop("La secuencia no coincide con corredor e indice del catalogo.",
      call. = FALSE)
  r <- data.table::data.table(
    codigo_variante = as.character(secuencias$codigo_variante),
    id_version_variante = as.character(secuencias$id_version_variante),
    codigo_linea = as.character(secuencias$codigo_linea),
    nombre_linea = as.character(secuencias$nombre_linea),
    sentido_variante = as.character(secuencias$sentido),
    indice_secuencia = as.integer(secuencias$indice_secuencia),
    punto_canonico_id = as.character(secuencias$punto_canonico_id),
    corredor_id = as.character(cp$corridor_id),
    indice_punto_corredor = as.integer(cp$corridor_point_index),
    nombre_via_fuente = as.character(cp$osm_name),
    zona_red = as.character(cp$network_zone),
    x_canonica = as.numeric(secuencias$x_canonica),
    y_canonica = as.numeric(secuencias$y_canonica))
  r[, `:=`(
    clave_via = identidad_via(corredor_id, nombre_via_fuente),
    nombre_via = data.table::fifelse(
      !is.na(nombre_via_fuente) & nzchar(trimws(nombre_via_fuente)),
      nombre_via_fuente, paste0("[Sin nombre] ", corredor_id)),
    nombre_via_disponible = !is.na(nombre_via_fuente) &
      nzchar(trimws(nombre_via_fuente)))]
  r[]
}

derivar_sentidos_ocurrencias <- function(secuencia, orientacion) {
  cols_o <- c("corredor_id", "eje_orientacion", "sentido_adelante",
    "sentido_atras", "confianza_orientacion", "fuente_orientacion",
    "sentido_aplicable")
  faltantes <- setdiff(c("id_version_variante", "corredor_id",
    "indice_secuencia", "indice_punto_corredor", "x_canonica", "y_canonica"),
    names(secuencia))
  if (length(faltantes)) stop(sprintf("Faltan columnas para derivar sentido: %s.",
    paste(faltantes, collapse = ", ")), call. = FALSE)
  if (length(setdiff(cols_o, names(orientacion))) ||
      anyDuplicated(orientacion$corredor_id))
    stop("La orientacion de corredores es invalida.", call. = FALSE)
  r <- data.table::copy(data.table::as.data.table(secuencia))
  o <- data.table::copy(data.table::as.data.table(orientacion)[, ..cols_o])
  r[, fila__ := .I]
  r <- merge(r, o, by = "corredor_id", all.x = TRUE, sort = FALSE)
  if (anyNA(r$eje_orientacion)) stop("Hay ocurrencias sin orientacion.",
    call. = FALSE)
  data.table::setorder(r, id_version_variante, indice_secuencia)
  r[, corrida__ := cumsum(seq_len(.N) == 1L |
    corredor_id != data.table::shift(corredor_id)), by = id_version_variante]
  r[, `:=`(xa__ = data.table::shift(x_canonica),
    ya__ = data.table::shift(y_canonica),
    xs__ = data.table::shift(x_canonica, type = "lead"),
    ys__ = data.table::shift(y_canonica, type = "lead")),
    by = id_version_variante]
  d <- r[, {
    direccion <- NULL
    if (!sentido_aplicable[[1L]]) {
      dx <- dy <- NA_real_; direccion <- "SIN_SENTIDO"
      metodo <- "corredor_sin_sentido"
    } else if (.N > 1L && indice_punto_corredor[.N] !=
        indice_punto_corredor[1L]) {
      dx <- x_canonica[.N] - x_canonica[1L]
      dy <- y_canonica[.N] - y_canonica[1L]
      direccion <- if (indice_punto_corredor[.N] > indice_punto_corredor[1L])
        sentido_adelante[[1L]] else sentido_atras[[1L]]
      metodo <- "orden_canonico_tramo"
    } else if (is.finite(xa__[[1L]]) && is.finite(xs__[[1L]])) {
      dx <- xs__[[1L]] - xa__[[1L]]; dy <- ys__[[1L]] - ya__[[1L]]
      metodo <- "tramo_unitario_contexto_ambos"
    } else if (is.finite(xs__[[1L]])) {
      dx <- xs__[[1L]] - x_canonica[[1L]]
      dy <- ys__[[1L]] - y_canonica[[1L]]
      metodo <- "tramo_unitario_contexto_siguiente"
    } else if (is.finite(xa__[[1L]])) {
      dx <- x_canonica[[1L]] - xa__[[1L]]
      dy <- y_canonica[[1L]] - ya__[[1L]]
      metodo <- "tramo_unitario_contexto_anterior"
    } else {
      dx <- dy <- NA_real_; metodo <- "tramo_unitario_sin_contexto"
    }
    if (is.null(direccion)) direccion <- if (
        eje_orientacion[[1L]] == "NS" && is.finite(dy) && dy != 0) {
      if (dy < 0) "N_S" else "S_N"
    } else if (eje_orientacion[[1L]] == "EO" && is.finite(dx) && dx != 0) {
      if (dx < 0) "E_O" else "O_E"
    } else "INDETERMINADO"
    if (direccion == "INDETERMINADO") metodo <- paste0(metodo,
      "_indeterminado")
    list(sentido_recorrido = direccion, metodo_sentido = metodo,
      delta_x_sentido = dx, delta_y_sentido = dy)
  }, by = .(id_version_variante, corredor_id, corrida__)]
  r <- d[r, on = c("id_version_variante", "corredor_id", "corrida__")]
  data.table::setorder(r, fila__)
  r[, c("fila__", "corrida__", "xa__", "ya__", "xs__", "ys__") := NULL]
  r[]
}

construir_tabla_presentacion <- function(secuencia, desplazamiento_m = 10) {
  r <- data.table::copy(data.table::as.data.table(secuencia))
  r[, id_presentacion := paste(punto_canonico_id, sentido_recorrido, sep = "::")]
  r[, x_mapa := x_canonica + data.table::fcase(
    sentido_recorrido == "S_N", desplazamiento_m,
    sentido_recorrido == "N_S", -desplazamiento_m, default = 0)]
  r[, y_mapa := y_canonica + data.table::fcase(
    sentido_recorrido == "E_O", desplazamiento_m,
    sentido_recorrido == "O_E", -desplazamiento_m, default = 0)]
  if (anyDuplicated(r[, .(id_version_variante, indice_secuencia,
      punto_canonico_id)])) stop("La presentacion contiene duplicados.",
    call. = FALSE)
  r[]
}

construir_catalogo_vias <- function(presentacion) {
  x <- unique(data.table::as.data.table(presentacion)[, .(clave_via,
    nombre_via, nombre_via_disponible, zona_red, corredor_id)])
  x[, zona := data.table::fcase(zona_red == "montevideo", "Montevideo",
    zona_red == "exterior", "Exterior", default = zona_red)]
  x[, .(
    etiqueta_via = if (all(!nombre_via_disponible)) paste0("[Sin nombre] ",
      sort(corredor_id)[[1L]], " · ", paste(sort(unique(zona)), collapse = "/"))
    else paste0(names(sort(table(nombre_via[nombre_via_disponible]),
      decreasing = TRUE))[[1L]], " · ", paste(sort(unique(zona)), collapse = "/")),
    corredores = data.table::uniqueN(corredor_id),
    con_nombre = any(nombre_via_disponible)), by = clave_via
  ][order(!con_nombre, etiqueta_via, clave_via)]
}

construir_registro_presentacion <- function(
    version_presentacion, archivo_secuencias, archivo_catalogo_puntos,
    archivo_referencia_orientacion_urbana, directorio_registro,
    version_variantes, snapshot_referencias, red_canonica,
    vias_sin_sentido = character(), desplazamiento_m = 10,
    esperados = NULL) {
  for (p in c("arrow", "data.table", "sf", "yaml"))
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("Falta instalar el paquete R '%s'.", p), call. = FALSE)
  entradas <- c(archivo_secuencias, archivo_catalogo_puntos,
    archivo_referencia_orientacion_urbana)
  if (any(!file.exists(entradas))) stop(sprintf("Faltan fuentes: %s",
    paste(entradas[!file.exists(entradas)], collapse = ", ")), call. = FALSE)
  destino <- file.path(directorio_registro, paste0("version=", version_presentacion))
  temporal <- file.path(directorio_registro,
    paste0(".preparando_version=", version_presentacion))
  if (dir.exists(destino) || dir.exists(temporal)) stop(sprintf(
    "El registro de presentacion ya existe o esta pendiente: %s",
    version_presentacion), call. = FALSE)
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  secuencias <- data.table::as.data.table(arrow::read_parquet(archivo_secuencias))
  puntos <- readRDS(archivo_catalogo_puntos)
  referencia <- readRDS(archivo_referencia_orientacion_urbana)
  secuencia <- preparar_secuencia_presentacion(secuencias, puntos)
  orientacion <- construir_orientacion_corredores(puntos, vias_sin_sentido,
    referencia)
  tabla <- construir_tabla_presentacion(
    derivar_sentidos_ocurrencias(secuencia, orientacion), desplazamiento_m)
  vias <- construir_catalogo_vias(tabla)
  metricas <- c(
    posiciones = nrow(tabla),
    posiciones_urbanas = sum(tabla$zona_red == "montevideo"),
    posiciones_urbanas_con_nombre = sum(tabla$zona_red == "montevideo" &
      tabla$nombre_via_disponible),
    corredores_exteriores_sin_nombre = data.table::uniqueN(
      tabla[zona_red == "exterior" & !nombre_via_disponible, corredor_id]),
    posiciones_exteriores_sin_nombre = sum(tabla$zona_red == "exterior" &
      !tabla$nombre_via_disponible),
    posiciones_sin_sentido = sum(tabla$sentido_recorrido == "SIN_SENTIDO"),
    posiciones_indeterminadas = sum(tabla$sentido_recorrido == "INDETERMINADO"),
    identidades_presentacion = data.table::uniqueN(tabla$id_presentacion))
  desplazamientos_ok <- all(tabla[sentido_recorrido == "N_S",
    x_mapa - x_canonica] == -desplazamiento_m) &&
    all(tabla[sentido_recorrido == "S_N", x_mapa - x_canonica] ==
      desplazamiento_m) &&
    all(tabla[sentido_recorrido == "E_O", y_mapa - y_canonica] ==
      desplazamiento_m) &&
    all(tabla[sentido_recorrido == "O_E", y_mapa - y_canonica] ==
      -desplazamiento_m) &&
    all(tabla[sentido_recorrido == "SIN_SENTIDO",
      x_mapa == x_canonica & y_mapa == y_canonica])
  regresion <- vapply(names(metricas), function(n) {
    esperado <- if (is.null(esperados[[n]])) NA_real_ else as.numeric(esperados[[n]])
    is.na(esperado) || metricas[[n]] == esperado
  }, logical(1L))
  controles <- data.table::data.table(
    control = c("ocurrencias_unicas", "claves_completas", "sentidos_validos",
      "orientacion_unica", "desplazamientos_validos",
      paste0("regresion_", names(metricas))),
    aprobado = c(!anyDuplicated(tabla[, .(id_version_variante,
      indice_secuencia, punto_canonico_id)]), !anyNA(tabla[, .(
      id_version_variante, indice_secuencia, punto_canonico_id, corredor_id,
      clave_via, nombre_via, zona_red, sentido_recorrido, id_presentacion,
      x_canonica, y_canonica, x_mapa, y_mapa)]),
      all(tabla$sentido_recorrido %in% c("N_S", "S_N", "E_O", "O_E",
        "SIN_SENTIDO", "INDETERMINADO")),
      !anyDuplicated(orientacion$corredor_id), desplazamientos_ok, regresion))
  controles[, estado := data.table::fifelse(aprobado, "aprobado", "fallo")]
  if (any(!controles$aprobado)) stop(sprintf("No aprobaron: %s.",
    paste(controles[!aprobado, control], collapse = ", ")), call. = FALSE)
  resumen <- data.table::data.table(indicador = names(metricas),
    valor = as.numeric(metricas))
  arrow::write_parquet(tabla, file.path(temporal,
    "secuencias_presentacion.parquet"), compression = "zstd")
  arrow::write_parquet(orientacion, file.path(temporal,
    "orientacion_corredores.parquet"), compression = "zstd")
  arrow::write_parquet(vias, file.path(temporal, "catalogo_vias.parquet"),
    compression = "zstd")
  data.table::fwrite(resumen, file.path(temporal, "resumen_presentacion.csv"))
  data.table::fwrite(controles, file.path(temporal, "controles.csv"))
  archivos <- list.files(temporal, full.names = TRUE)
  data.table::fwrite(data.table::data.table(archivo = basename(archivos),
    bytes = as.numeric(file.info(archivos)$size),
    md5 = unname(tools::md5sum(archivos))),
    file.path(temporal, "manifiesto_archivos.csv"))
  yaml::write_yaml(list(tipo = "registro_presentacion",
    version_presentacion = version_presentacion,
    version_registro_variantes = version_variantes,
    snapshot_referencias = snapshot_referencias, red_canonica = red_canonica,
    dependencia_od = FALSE, desplazamiento_m = desplazamiento_m,
    vias_sin_sentido = as.list(as.character(vias_sin_sentido)),
    posiciones = nrow(tabla),
    identidades_presentacion = data.table::uniqueN(tabla$id_presentacion),
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    file.path(temporal, "manifiesto.yml"))
  if (!file.rename(temporal, destino)) stop(
    "No se pudo promover atomicamente el registro de presentacion.",
    call. = FALSE)
  list(destino = destino, resumen = resumen, controles = controles)
}
