colapsar_valores_uso <- function(valores) {
  valores <- sort(unique(as.character(valores[!is.na(valores)])))
  paste(valores, collapse = "|")
}

resumir_uso_variantes_lado <- function(
    od,
    columna_variante,
    columna_linea,
    columna_recorrido,
    sufijo) {
  datos <- data.table::copy(od)
  datos[, codigo_variante := as.character(get(columna_variante))]
  resumen <- datos[, .(
    registros = .N,
    personas = sum(as.numeric(cantidad_pasajeros), na.rm = TRUE),
    recorridos = data.table::uniqueN(get(columna_recorrido), na.rm = TRUE),
    lineas = colapsar_valores_uso(get(columna_linea))
  ), by = codigo_variante]
  data.table::setnames(
    resumen,
    c("registros", "personas", "recorridos", "lineas"),
    paste0(c("registros", "personas", "recorridos", "lineas"), "_", sufijo)
  )
  resumen
}

resolver_uso_variantes_dia <- function(
    fecha,
    archivo_od_operativa,
    directorio_registro_canonico,
    directorio_salida) {
  for (paquete in c("arrow", "data.table", "yaml")) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete R '%s'.", paquete), call. = FALSE)
    }
  }
  fecha_date <- as.Date(fecha)
  if (length(fecha) != 1L || is.na(fecha_date) || format(fecha_date, "%Y-%m-%d") != fecha) {
    stop("La fecha debe tener formato AAAA-MM-DD.", call. = FALSE)
  }
  destino <- file.path(directorio_salida, "uso_variantes", paste0("fecha=", fecha))
  if (dir.exists(destino) || file.exists(destino)) {
    stop(sprintf("El uso diario de variantes ya existe y no sera sobrescrito: %s", destino), call. = FALSE)
  }
  archivo_variantes <- file.path(directorio_registro_canonico, "variantes.parquet")
  archivo_manifiesto <- file.path(directorio_registro_canonico, "manifiesto.yml")
  entradas <- c(archivo_od_operativa, archivo_variantes, archivo_manifiesto)
  faltantes <- entradas[!file.exists(entradas)]
  if (length(faltantes)) {
    stop(sprintf("Faltan archivos de entrada: %s", paste(faltantes, collapse = ", ")), call. = FALSE)
  }

  manifiesto_registro <- yaml::read_yaml(archivo_manifiesto)
  if (!identical(manifiesto_registro$etapa, "registro_canonico") ||
      !identical(manifiesto_registro$estado, "completo")) {
    stop("El registro de variantes canonicas no esta promovido o esta incompleto.", call. = FALSE)
  }
  metadatos_referencia <- c(
    manifiesto_registro$version,
    manifiesto_registro$snapshot_referencias,
    manifiesto_registro$red_canonica
  )
  if (length(metadatos_referencia) != 3L || anyNA(metadatos_referencia) ||
      any(!nzchar(as.character(metadatos_referencia)))) {
    stop("El manifiesto canonico no identifica version, snapshot y red.", call. = FALSE)
  }

  columnas_od <- c(
    "sevar_codigo_subida", "linea_codigo_subida", "numero_evento_real_subida",
    "sevar_codigo_bajada", "linea_codigo_bajada", "numero_evento_real_bajada",
    "cantidad_pasajeros"
  )
  dataset_od <- arrow::open_dataset(archivo_od_operativa, format = "parquet")
  faltantes_od <- setdiff(columnas_od, names(dataset_od$schema))
  if (length(faltantes_od)) {
    stop(sprintf(
      "Faltan columnas en la OD operativa: %s",
      paste(faltantes_od, collapse = ", ")
    ), call. = FALSE)
  }
  od <- data.table::as.data.table(arrow::read_parquet(
    archivo_od_operativa,
    col_select = tidyselect::all_of(columnas_od)
  ))
  if (!nrow(od)) stop("La OD operativa esta vacia.", call. = FALSE)

  registro <- data.table::as.data.table(arrow::read_parquet(
    archivo_variantes,
    col_select = c(
      "codigo_variante", "id_version_variante", "codigo_linea",
      "codigo_empresa", "codigo_subsistema", "sentido",
      "fecha_vigencia", "fecha_baja"
    )
  ))
  registro[, `:=`(
    codigo_variante = as.character(codigo_variante),
    id_version_variante = as.character(id_version_variante)
  )]
  if (anyDuplicated(registro$codigo_variante)) {
    stop("El registro canonico contiene mas de una version por codigo en esta entrega.", call. = FALSE)
  }
  if (anyDuplicated(registro$id_version_variante)) {
    stop("El registro canonico contiene id_version_variante duplicados.", call. = FALSE)
  }

  subida <- resumir_uso_variantes_lado(
    od, "sevar_codigo_subida", "linea_codigo_subida",
    "numero_evento_real_subida", "subida"
  )
  bajada <- resumir_uso_variantes_lado(
    od, "sevar_codigo_bajada", "linea_codigo_bajada",
    "numero_evento_real_bajada", "bajada"
  )
  codigos_observados <- unique(c(subida$codigo_variante, bajada$codigo_variante))
  uso <- data.table::data.table(codigo_variante = codigos_observados)
  uso <- merge(uso, subida, by = "codigo_variante", all.x = TRUE, sort = FALSE)
  uso <- merge(uso, bajada, by = "codigo_variante", all.x = TRUE, sort = FALSE)

  columnas_cero <- c(
    "registros_subida", "personas_subida", "recorridos_subida",
    "registros_bajada", "personas_bajada", "recorridos_bajada"
  )
  for (columna in columnas_cero) {
    data.table::set(uso, which(is.na(uso[[columna]])), columna, 0)
  }
  for (columna in c("lineas_subida", "lineas_bajada")) {
    data.table::set(uso, which(is.na(uso[[columna]])), columna, "")
  }

  columnas_registro <- c(
    "codigo_variante", "id_version_variante", "codigo_linea",
    "codigo_empresa", "codigo_subsistema", "sentido",
    "fecha_vigencia", "fecha_baja"
  )
  uso <- merge(
    uso, registro[, ..columnas_registro],
    by = "codigo_variante", all.x = TRUE, sort = FALSE
  )
  uso[, `:=`(
    fecha_operativa = fecha,
    version_registro_variantes = as.character(manifiesto_registro$version),
    snapshot_referencias = as.character(manifiesto_registro$snapshot_referencias),
    red_canonica = as.character(manifiesto_registro$red_canonica),
    estado_resolucion = data.table::fifelse(
      is.na(id_version_variante), "no_resuelta_registro", "resuelta"
    )
  )]
  data.table::setcolorder(uso, c(
    "fecha_operativa", "codigo_variante", "id_version_variante",
    "estado_resolucion", "version_registro_variantes",
    "snapshot_referencias", "red_canonica",
    setdiff(names(uso), c(
      "fecha_operativa", "codigo_variante", "id_version_variante",
      "estado_resolucion", "version_registro_variantes",
      "snapshot_referencias", "red_canonica"
    ))
  ))
  data.table::setorder(uso, estado_resolucion, codigo_variante, na.last = TRUE)

  controles <- data.table::data.table(
    control = c(
      "variantes_diarias_unicas", "universo_od_conservado",
      "versiones_resueltas_existen", "metadatos_referencia_completos",
      "registros_subida_conservados", "registros_bajada_conservados",
      "personas_subida_conservadas", "personas_bajada_conservadas"
    ),
    aprobado = c(
      anyDuplicated(uso$codigo_variante) == 0L,
      setequal(codigos_observados, uso$codigo_variante),
      all(uso[estado_resolucion == "resuelta", id_version_variante] %in%
            registro$id_version_variante),
      all(nzchar(uso$version_registro_variantes)) &&
        all(nzchar(uso$snapshot_referencias)) && all(nzchar(uso$red_canonica)),
      sum(uso$registros_subida) == nrow(od),
      sum(uso$registros_bajada) == nrow(od),
      isTRUE(all.equal(
        sum(uso$personas_subida),
        sum(as.numeric(od$cantidad_pasajeros), na.rm = TRUE)
      )),
      isTRUE(all.equal(
        sum(uso$personas_bajada),
        sum(as.numeric(od$cantidad_pasajeros), na.rm = TRUE)
      ))
    )
  )
  controles[, estado := data.table::fifelse(aprobado, "aprobado", "fallo")]
  if (any(!controles$aprobado)) {
    stop("La relacion diaria de variantes no aprobo sus controles.", call. = FALSE)
  }

  no_resueltas <- uso[estado_resolucion == "no_resuelta_registro"]
  resumen <- uso[, .(
    variantes = .N,
    registros_subida = sum(registros_subida),
    personas_subida = sum(personas_subida),
    recorridos_subida = sum(recorridos_subida),
    registros_bajada = sum(registros_bajada),
    personas_bajada = sum(personas_bajada),
    recorridos_bajada = sum(recorridos_bajada)
  ), by = estado_resolucion][order(estado_resolucion)]
  resumen <- data.table::rbindlist(list(resumen, uso[, .(
    estado_resolucion = "TOTAL",
    variantes = .N,
    registros_subida = sum(registros_subida),
    personas_subida = sum(personas_subida),
    recorridos_subida = sum(recorridos_subida),
    registros_bajada = sum(registros_bajada),
    personas_bajada = sum(personas_bajada),
    recorridos_bajada = sum(recorridos_bajada)
  )]), use.names = TRUE)

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- file.path(
    dirname(destino),
    sprintf(".tmp_fecha_%s_%s", gsub("-", "", fecha), Sys.getpid())
  )
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  terminado <- FALSE
  on.exit({
    if (!terminado && dir.exists(temporal)) {
      unlink(temporal, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  arrow::write_parquet(
    uso, file.path(temporal, "uso_variantes.parquet"), compression = "zstd"
  )
  data.table::fwrite(no_resueltas, file.path(temporal, "variantes_no_resueltas.csv"))
  data.table::fwrite(resumen, file.path(temporal, "resumen_uso_variantes.csv"))
  data.table::fwrite(controles, file.path(temporal, "controles.csv"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    tipo_salida = "relacion_diaria_liviana",
    fuente_od = normalizePath(archivo_od_operativa, winslash = "/", mustWork = TRUE),
    registro_canonico = normalizePath(
      directorio_registro_canonico, winslash = "/", mustWork = TRUE
    ),
    version_registro_variantes = manifiesto_registro$version,
    snapshot_referencias = manifiesto_registro$snapshot_referencias,
    red_canonica = manifiesto_registro$red_canonica,
    variantes_observadas = nrow(uso),
    variantes_resueltas = uso[estado_resolucion == "resuelta", .N],
    variantes_no_resueltas = nrow(no_resueltas),
    incluye_geometrias = FALSE,
    incluye_secuencias = FALSE,
    incluye_paradas = FALSE,
    controles_aprobados = sum(controles$aprobado),
    controles_fallidos = sum(!controles$aprobado),
    creado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), file.path(temporal, "manifiesto.yml"))
  if (!file.rename(temporal, destino)) {
    stop(sprintf("No se pudo promover el uso diario de variantes: %s", destino), call. = FALSE)
  }
  terminado <- TRUE
  list(
    destino = destino,
    uso = uso,
    no_resueltas = no_resueltas,
    resumen = resumen,
    controles = controles
  )
}
