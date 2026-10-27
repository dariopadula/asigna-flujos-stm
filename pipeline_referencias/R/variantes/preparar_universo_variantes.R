validar_wkt_referencias <- function(wkt) {
  resultado <- rep(FALSE, length(wkt))
  indices <- which(!is.na(wkt) & nzchar(trimws(wkt)))
  if (!length(indices)) return(resultado)
  geometrias <- tryCatch(sf::st_as_sfc(wkt[indices]), error = function(e) NULL)
  if (!is.null(geometrias)) {
    resultado[indices] <- !sf::st_is_empty(geometrias) & sf::st_is_valid(geometrias)
    return(resultado)
  }
  resultado[indices] <- vapply(indices, function(indice) {
    geometria <- tryCatch(sf::st_as_sfc(wkt[[indice]]), error = function(e) NULL)
    !is.null(geometria) && !sf::st_is_empty(geometria)[[1]] && sf::st_is_valid(geometria)[[1]]
  }, logical(1))
  resultado
}

preparar_universo_variantes <- function(
    snapshot_id,
    fecha_corte,
    anio_mes,
    subsistemas,
    archivo_geometrias,
    archivos_paradas,
    archivo_catalogo_lineas,
    directorio_salida,
    variantes_esperadas = NULL,
    paradas_esperadas = NULL) {
  for (paquete in c("arrow", "data.table", "sf", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  corte <- as.Date(fecha_corte)
  mes_objetivo <- as.integer(anio_mes)
  if (is.na(corte) || format(corte, "%Y-%m-%d") != fecha_corte) {
    stop("La fecha de corte debe tener formato AAAA-MM-DD.", call. = FALSE)
  }
  entradas <- c(archivo_geometrias, archivos_paradas, archivo_catalogo_lineas)
  inexistentes <- entradas[!file.exists(entradas)]
  if (length(inexistentes)) {
    stop(sprintf("Faltan archivos de entrada: %s", paste(inexistentes, collapse = ", ")), call. = FALSE)
  }
  destino <- file.path(directorio_salida, paste0("entrega=", snapshot_id))
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf("La entrega ya existe y no sera sobrescrita: %s", destino), call. = FALSE)
  }

  columnas_paradas <- c(
    "cod_ubic_parada", "desc_ubic_parada", "cod_nombre_via_1",
    "cod_nombre_via_2", "nom_calle", "cod_variante", "ordinal",
    "cod_linea", "anio_mes", "x", "y"
  )
  paradas <- data.table::rbindlist(lapply(archivos_paradas, function(archivo) {
    datos <- data.table::fread(
      archivo, select = columnas_paradas,
      showProgress = FALSE, encoding = "UTF-8"
    )
    datos[anio_mes == mes_objetivo]
  }), fill = TRUE)
  paradas <- unique(
    paradas,
    by = c("cod_variante", "cod_ubic_parada", "ordinal", "x", "y")
  )

  lineas <- data.table::fread(archivo_catalogo_lineas, showProgress = FALSE, encoding = "UTF-8")
  data.table::setnames(lineas, sub("^lineas_metro[.]", "", names(lineas)))
  lineas_validas <- lineas[
    anio_mes == mes_objetivo & codigo_subsistema %in% as.integer(subsistemas)
  ]
  pares_variante_linea <- unique(paradas[, .(
    codigo = cod_variante,
    codigo_linea = cod_linea
  )])
  variantes_linea <- lineas_validas[
    pares_variante_linea,
    on = .(codigo = codigo_linea),
    nomatch = 0L
  ][, .(
    codigo = as.character(i.codigo),
    codigo_linea = as.character(codigo),
    nombre_linea = nombre_publico,
    codigo_empresa = cod_empresa,
    codigo_subsistema
  )]
  if (anyDuplicated(variantes_linea$codigo)) {
    stop("Una variante quedo asociada a mas de una linea valida.", call. = FALSE)
  }

  geometrias <- data.table::fread(
    archivo_geometrias,
    select = c(
      "codigo", "sentido", "subli_codigo", "fecha_vigencia", "fecha_baja",
      "nombre_variante", "the_geom"
    ),
    showProgress = FALSE,
    encoding = "UTF-8"
  )
  geometrias[, `:=`(
    codigo = as.character(codigo),
    fila_fuente = seq_len(.N),
    vigencia_date = as.Date(substr(fecha_vigencia, 1L, 10L)),
    baja_date = as.Date(substr(fecha_baja, 1L, 10L))
  )]
  geometrias <- geometrias[codigo %in% variantes_linea$codigo]
  codigos_con_registro <- unique(geometrias$codigo)
  al_corte <- geometrias[!is.na(vigencia_date) & vigencia_date <= corte]
  data.table::setorder(al_corte, codigo, vigencia_date, fila_fuente)
  al_corte <- al_corte[, .SD[.N], by = codigo]
  al_corte[, `:=`(
    geometria_activa = is.na(baja_date) | baja_date >= corte,
    geometria_wkt_presente = !is.na(the_geom) & nzchar(trimws(the_geom))
  )]
  al_corte[, geometria_valida := geometria_wkt_presente & validar_wkt_referencias(the_geom)]

  estado <- merge(
    variantes_linea[, .(codigo)],
    al_corte[, .(
      codigo,
      fecha_vigencia = as.character(vigencia_date),
      fecha_baja = as.character(baja_date),
      geometria_activa,
      geometria_wkt_presente,
      geometria_valida
    )],
    by = "codigo",
    all.x = TRUE
  )
  estado[, estado_referencia := data.table::fcase(
    !codigo %in% codigos_con_registro, "sin_registro_geometria",
    is.na(fecha_vigencia), "sin_version_al_corte",
    !geometria_activa, "dada_de_baja",
    !geometria_wkt_presente, "sin_wkt",
    !geometria_valida, "wkt_invalido",
    default = "construible"
  )]

  construibles <- al_corte[
    geometria_activa & geometria_valida
  ][variantes_linea, on = "codigo", nomatch = 0L]
  construibles[, id_version_variante := paste0(
    codigo, "_", format(vigencia_date, "%Y%m%d")
  )]
  if (anyDuplicated(construibles$codigo) || anyDuplicated(construibles$id_version_variante)) {
    stop("El universo construible no tiene una fila unica por variante y version.", call. = FALSE)
  }
  universo <- construibles[, .(
    codigo_variante = codigo,
    id_version_variante,
    codigo_linea,
    nombre_linea,
    codigo_empresa,
    codigo_subsistema,
    sentido,
    subli_codigo,
    nombre_variante,
    fecha_vigencia = as.character(vigencia_date),
    fecha_baja = as.character(baja_date),
    geometria_wkt = the_geom
  )]
  data.table::setorder(universo, codigo_variante)
  paradas_universo <- paradas[as.character(cod_variante) %in% universo$codigo_variante]
  data.table::setorder(paradas_universo, cod_variante, ordinal, cod_ubic_parada, na.last = TRUE)
  pendientes <- estado[estado_referencia != "construible"]

  if (!is.null(variantes_esperadas) && nrow(universo) != as.integer(variantes_esperadas)) {
    stop(sprintf(
      "El universo produjo %s variantes; se esperaban %s.",
      nrow(universo), variantes_esperadas
    ), call. = FALSE)
  }
  if (!is.null(paradas_esperadas) && nrow(paradas_universo) != as.integer(paradas_esperadas)) {
    stop(sprintf(
      "El universo produjo %s paradas; se esperaban %s.",
      nrow(paradas_universo), paradas_esperadas
    ), call. = FALSE)
  }

  resumen <- estado[, .(variantes = .N), by = estado_referencia][order(estado_referencia)]
  resumen <- data.table::rbindlist(list(resumen, data.table::data.table(
    estado_referencia = "TOTAL_CANDIDATAS",
    variantes = nrow(estado)
  )))

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(dirname(destino), paste0(".tmp_", snapshot_id, "_", Sys.getpid()))
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) unlink(temporal, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  arrow::write_parquet(universo, file.path(temporal, "universo_variantes.parquet"), compression = "zstd")
  arrow::write_parquet(paradas_universo, file.path(temporal, "paradas_universo.parquet"), compression = "zstd")
  data.table::fwrite(pendientes, file.path(temporal, "variantes_pendientes.csv"))
  data.table::fwrite(resumen, file.path(temporal, "resumen_universo.csv"))
  yaml::write_yaml(list(
    snapshot_id = snapshot_id,
    fecha_corte = fecha_corte,
    anio_mes = mes_objetivo,
    subsistemas = as.integer(subsistemas),
    dependencia_od = FALSE,
    variantes_construibles = nrow(universo),
    variantes_pendientes = nrow(pendientes),
    filas_paradas = nrow(paradas_universo),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))
  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover la salida temporal a: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  list(destino = destino, universo = universo, pendientes = pendientes, resumen = resumen)
}
