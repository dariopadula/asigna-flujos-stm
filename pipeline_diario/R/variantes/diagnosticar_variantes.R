validar_wkt_variantes <- function(wkt) {
  resultado <- rep(FALSE, length(wkt))
  candidatas <- which(!is.na(wkt) & nzchar(trimws(wkt)))
  if (!length(candidatas)) return(resultado)

  geometria <- tryCatch(sf::st_as_sfc(wkt[candidatas]), error = function(e) NULL)
  if (!is.null(geometria)) {
    resultado[candidatas] <- !sf::st_is_empty(geometria) & sf::st_is_valid(geometria)
    return(resultado)
  }

  resultado[candidatas] <- vapply(candidatas, function(indice) {
    geometria_individual <- tryCatch(sf::st_as_sfc(wkt[[indice]]), error = function(e) NULL)
    !is.null(geometria_individual) &&
      !sf::st_is_empty(geometria_individual)[[1]] &&
      sf::st_is_valid(geometria_individual)[[1]]
  }, logical(1))
  resultado
}

colapsar_codigos <- function(x) {
  valores <- sort(unique(as.character(x[!is.na(x) & nzchar(as.character(x))])))
  if (length(valores)) paste(valores, collapse = "|") else NA_character_
}

diagnosticar_variantes <- function(
    fecha,
    archivo_od_operativa,
    archivo_geometrias,
    archivos_paradas,
    archivo_catalogo_lineas,
    directorio_salida,
    subsistemas = c(1L, 2L, 3L, 4L)) {
  for (paquete in c("arrow", "data.table", "sf", "tidyselect", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }

  fecha_date <- as.Date(fecha)
  if (length(fecha) != 1L || is.na(fecha_date) || format(fecha_date, "%Y-%m-%d") != fecha) {
    stop("La fecha debe tener formato AAAA-MM-DD.", call. = FALSE)
  }
  entradas <- c(archivo_od_operativa, archivo_geometrias, archivos_paradas, archivo_catalogo_lineas)
  inexistentes <- entradas[!file.exists(entradas)]
  if (length(inexistentes)) {
    stop(sprintf("Faltan archivos de entrada: %s", paste(inexistentes, collapse = ", ")), call. = FALSE)
  }

  destino <- file.path(directorio_salida, "diagnostico_variantes", paste0("fecha=", fecha))
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf("El diagnostico ya existe y no sera sobrescrito: %s", destino), call. = FALSE)
  }

  columnas_od <- c(
    "sevar_codigo_subida", "sevar_codigo_bajada", "linea_codigo_subida",
    "linea_codigo_bajada", "cantidad_pasajeros"
  )
  od <- data.table::as.data.table(arrow::read_parquet(
    archivo_od_operativa,
    col_select = tidyselect::all_of(columnas_od)
  ))
  variantes_subida_na <- sum(is.na(od$sevar_codigo_subida))
  variantes_bajada_na <- sum(is.na(od$sevar_codigo_bajada))

  uso_subida <- od[!is.na(sevar_codigo_subida), .(
    registros_subida = .N,
    personas_subida = sum(as.numeric(cantidad_pasajeros), na.rm = TRUE),
    lineas_subida_od = colapsar_codigos(linea_codigo_subida)
  ), by = .(codigo_variante = as.character(sevar_codigo_subida))]
  uso_bajada <- od[!is.na(sevar_codigo_bajada), .(
    registros_bajada = .N,
    personas_bajada = sum(as.numeric(cantidad_pasajeros), na.rm = TRUE),
    lineas_bajada_od = colapsar_codigos(linea_codigo_bajada)
  ), by = .(codigo_variante = as.character(sevar_codigo_bajada))]
  diagnostico <- merge(uso_subida, uso_bajada, by = "codigo_variante", all = TRUE)
  for (columna in c("registros_subida", "personas_subida", "registros_bajada", "personas_bajada")) {
    data.table::set(diagnostico, which(is.na(diagnostico[[columna]])), columna, 0)
  }

  pares_od <- unique(data.table::rbindlist(list(
    od[!is.na(sevar_codigo_subida) & !is.na(linea_codigo_subida), .(
      codigo_variante = as.character(sevar_codigo_subida),
      codigo_linea = as.character(linea_codigo_subida)
    )],
    od[!is.na(sevar_codigo_bajada) & !is.na(linea_codigo_bajada), .(
      codigo_variante = as.character(sevar_codigo_bajada),
      codigo_linea = as.character(linea_codigo_bajada)
    )]
  )))

  geometrias_raw <- data.table::fread(
    archivo_geometrias,
    select = c("codigo", "sentido", "fecha_vigencia", "fecha_baja", "nombre_variante", "the_geom"),
    showProgress = FALSE,
    encoding = "UTF-8"
  )
  geometrias_raw[, codigo_variante := as.character(codigo)]
  geometrias_raw <- geometrias_raw[codigo_variante %in% diagnostico$codigo_variante]
  codigos_con_registro <- unique(geometrias_raw$codigo_variante)
  geometrias_raw[, `:=`(
    vigencia_date = as.Date(substr(fecha_vigencia, 1L, 10L)),
    baja_date = as.Date(substr(fecha_baja, 1L, 10L))
  )]
  geometria_al_corte <- geometrias_raw[!is.na(vigencia_date) & vigencia_date <= fecha_date]
  data.table::setorder(geometria_al_corte, codigo_variante, vigencia_date)
  geometria_al_corte <- geometria_al_corte[, .SD[.N], by = codigo_variante]
  geometria_al_corte[, `:=`(
    geometria_activa = is.na(baja_date) | baja_date >= fecha_date,
    geometria_wkt_presente = !is.na(the_geom) & nzchar(trimws(the_geom))
  )]
  geometria_al_corte[, geometria_valida := geometria_activa &
    geometria_wkt_presente & validar_wkt_variantes(the_geom)]
  geometria_estado <- geometria_al_corte[, .(
    codigo_variante,
    sentido_geometria = sentido,
    nombre_variante,
    fecha_vigencia = as.character(vigencia_date),
    fecha_baja = as.character(baja_date),
    geometria_activa,
    geometria_wkt_presente,
    geometria_valida
  )]
  diagnostico <- merge(diagnostico, geometria_estado, by = "codigo_variante", all.x = TRUE)
  diagnostico[, geometria_estado := data.table::fcase(
    !codigo_variante %in% codigos_con_registro, "sin_registro",
    is.na(fecha_vigencia), "sin_version_al_corte",
    !geometria_activa, "dada_de_baja",
    !geometria_wkt_presente, "sin_wkt",
    !geometria_valida, "wkt_invalido",
    default = "vigente_valida"
  )]

  anio_mes_objetivo <- as.integer(format(fecha_date, "%Y%m"))
  paradas <- data.table::rbindlist(lapply(archivos_paradas, function(archivo) {
    datos <- data.table::fread(
      archivo,
      select = c("cod_ubic_parada", "cod_variante", "ordinal", "cod_linea", "anio_mes", "x", "y"),
      showProgress = FALSE,
      encoding = "UTF-8"
    )
    datos[anio_mes == anio_mes_objetivo]
  }), fill = TRUE)
  paradas[, codigo_variante := as.character(cod_variante)]
  paradas <- unique(paradas[
    codigo_variante %in% diagnostico$codigo_variante
  ], by = c("codigo_variante", "cod_ubic_parada", "ordinal", "x", "y"))

  estadisticas_paradas <- paradas[, {
    ordinales <- sort(unique(as.integer(ordinal[!is.na(ordinal)])))
    .(
      filas_paradas = .N,
      paradas_distintas = data.table::uniqueN(cod_ubic_parada, na.rm = TRUE),
      ordinales_distintos = length(ordinales),
      ordinales_faltantes = sum(is.na(ordinal)),
      ordinales_duplicados = sum(duplicated(ordinal[!is.na(ordinal) & ordinal > 0L])),
      saltos_ordinal = if (length(ordinales) > 1L) sum(diff(ordinales) > 1L) else 0L,
      lineas_paradas = colapsar_codigos(cod_linea)
    )
  }, by = codigo_variante]
  diagnostico <- merge(diagnostico, estadisticas_paradas, by = "codigo_variante", all.x = TRUE)
  for (columna in c(
    "filas_paradas", "paradas_distintas", "ordinales_distintos",
    "ordinales_faltantes", "ordinales_duplicados", "saltos_ordinal"
  )) {
    data.table::set(diagnostico, which(is.na(diagnostico[[columna]])), columna, 0)
  }

  catalogo <- data.table::fread(archivo_catalogo_lineas, showProgress = FALSE, encoding = "UTF-8")
  data.table::setnames(catalogo, sub("^lineas_metro[.]", "", names(catalogo)))
  lineas_validas <- unique(as.character(catalogo[
    anio_mes == anio_mes_objetivo & codigo_subsistema %in% subsistemas,
    codigo
  ]))
  pares_paradas <- unique(paradas[!is.na(cod_linea), .(
    codigo_variante,
    codigo_linea = as.character(cod_linea)
  )])
  pares_od[, `:=`(
    linea_en_catalogo = codigo_linea %in% lineas_validas,
    linea_en_paradas = paste(codigo_variante, codigo_linea, sep = "|") %in%
      paste(pares_paradas$codigo_variante, pares_paradas$codigo_linea, sep = "|")
  )]
  inconsistencias_linea <- pares_od[!linea_en_catalogo | !linea_en_paradas]
  resumen_inconsistencias <- inconsistencias_linea[, .(
    lineas_od_sin_catalogo = colapsar_codigos(codigo_linea[!linea_en_catalogo]),
    lineas_od_sin_paradas = colapsar_codigos(codigo_linea[!linea_en_paradas])
  ), by = codigo_variante]
  diagnostico <- merge(diagnostico, resumen_inconsistencias, by = "codigo_variante", all.x = TRUE)

  diagnostico[, `:=`(
    tiene_geometria_vigente = geometria_estado == "vigente_valida",
    tiene_secuencia_paradas = filas_paradas > 0L & ordinales_faltantes == 0L,
    secuencia_consistente = filas_paradas > 0L & ordinales_faltantes == 0L &
      ordinales_duplicados == 0L,
    linea_consistente = is.na(lineas_od_sin_catalogo) & is.na(lineas_od_sin_paradas)
  )]
  diagnostico[, estado_diagnostico := data.table::fcase(
    !tiene_geometria_vigente & filas_paradas == 0L, "sin_geometria_y_paradas",
    !tiene_geometria_vigente, "sin_geometria_vigente",
    filas_paradas == 0L, "sin_paradas_mes",
    !secuencia_consistente, "secuencia_paradas_revisar",
    !linea_consistente, "linea_variante_inconsistente",
    default = "completa"
  )]
  data.table::setorder(diagnostico, estado_diagnostico, codigo_variante)

  resumen <- diagnostico[, .(
    variantes = .N,
    registros_subida = sum(registros_subida),
    registros_bajada = sum(registros_bajada)
  ), by = estado_diagnostico][order(estado_diagnostico)]
  resumen <- data.table::rbindlist(list(
    resumen,
    data.table::data.table(
      estado_diagnostico = "TOTAL",
      variantes = nrow(diagnostico),
      registros_subida = sum(diagnostico$registros_subida),
      registros_bajada = sum(diagnostico$registros_bajada)
    )
  ), use.names = TRUE)

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(dirname(destino), sprintf(".tmp_fecha_%s_%s", gsub("-", "", fecha), Sys.getpid()))
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) unlink(temporal, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  data.table::fwrite(diagnostico, file.path(temporal, "diagnostico_variantes.csv"))
  data.table::fwrite(
    diagnostico[estado_diagnostico != "completa"],
    file.path(temporal, "variantes_con_problemas.csv")
  )
  data.table::fwrite(inconsistencias_linea, file.path(temporal, "inconsistencias_linea_variante.csv"))
  data.table::fwrite(resumen, file.path(temporal, "resumen_diagnostico.csv"))
  yaml::write_yaml(list(
    fecha_objetivo = fecha,
    anio_mes = anio_mes_objetivo,
    variantes_union = nrow(diagnostico),
    variantes_subida = nrow(uso_subida),
    variantes_bajada = nrow(uso_bajada),
    registros_variante_subida_na = variantes_subida_na,
    registros_variante_bajada_na = variantes_bajada_na,
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))

  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover la salida temporal a: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  list(destino = destino, resumen = resumen, diagnostico = diagnostico)
}

