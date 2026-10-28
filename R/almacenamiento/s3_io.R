.s3_io_cache <- new.env(parent = emptyenv())

leer_variable_s3 <- function(nombre, obligatoria = TRUE) {
  valor <- Sys.getenv(nombre, unset = "")
  if (obligatoria && !nzchar(valor)) {
    stop(sprintf("Falta definir la variable de ambiente %s.", nombre),
      call. = FALSE)
  }
  valor
}

obtener_configuracion_s3 <- function() {
  list(
    connection_name = leer_variable_s3("ASIGNA_FLUJOS_S3_CONNECTION"),
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)
  )
}

obtener_cliente_s3 <- function(
    connection_name = leer_variable_s3("ASIGNA_FLUJOS_S3_CONNECTION")) {
  cliente_cacheado <- .s3_io_cache[[connection_name]]
  if (!is.null(cliente_cacheado)) return(cliente_cacheado)
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Se requiere el paquete R 'reticulate' para conectar a S3.",
      call. = FALSE)
  }
  cmldata <- reticulate::import("cml.data_v1", delay_load = FALSE)
  conexion <- cmldata$get_connection(connection_name)
  cliente <- conexion$get_base_connection()
  .s3_io_cache[[connection_name]] <- cliente
  cliente
}

unir_key_s3 <- function(...) {
  partes <- unlist(list(...), use.names = FALSE)
  if (!length(partes)) return("")
  if (!is.character(partes) || anyNA(partes)) {
    stop("Las partes de una clave S3 deben ser texto no nulo.", call. = FALSE)
  }
  partes <- gsub("\\\\", "/", partes)
  partes <- gsub("^/+|/+$", "", partes)
  partes <- partes[nzchar(partes)]
  paste(partes, collapse = "/")
}

descomponer_uri_s3 <- function(uri) {
  if (length(uri) != 1L || is.na(uri) ||
      !grepl("^s3[a-z0-9+.-]*://[^/]+", uri)) {
    stop(sprintf("No es una URI S3 valida: %s.", uri), call. = FALSE)
  }
  sin_esquema <- sub("^s3[a-z0-9+.-]*://", "", sub("/+$", "", uri))
  separador <- regexpr("/", sin_esquema, fixed = TRUE)[[1L]]
  if (separador < 0L) {
    return(list(bucket = sin_esquema, prefix = ""))
  }
  list(
    bucket = substr(sin_esquema, 1L, separador - 1L),
    prefix = unir_key_s3(substr(sin_esquema, separador + 1L, nchar(sin_esquema)))
  )
}

ruta_local_relativa <- function(raiz, relativa) {
  partes <- strsplit(gsub("\\\\", "/", relativa), "/", fixed = TRUE)[[1L]]
  if (!length(partes) || any(!nzchar(partes)) || any(partes %in% c(".", ".."))) {
    stop(sprintf("Ruta relativa insegura: %s.", relativa), call. = FALSE)
  }
  do.call(file.path, c(list(raiz), as.list(partes)))
}

key_absoluta_s3 <- function(
    ruta_relativa,
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  unir_key_s3(prefix, ruta_relativa)
}

listar_objetos_s3 <- function(
    cliente,
    prefijo_relativo = "",
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  prefijo <- key_absoluta_s3(prefijo_relativo, prefix)
  token <- NULL
  keys <- character()

  repeat {
    argumentos <- list(Bucket = bucket, Prefix = prefijo)
    if (!is.null(token) && nzchar(token)) {
      argumentos$ContinuationToken <- token
    }
    respuesta <- do.call(cliente$list_objects_v2, argumentos)
    objetos <- respuesta$Contents
    if (!is.null(objetos) && length(objetos)) {
      keys <- c(keys, vapply(
        objetos, function(objeto) as.character(objeto$Key), character(1L)
      ))
    }
    if (!isTRUE(respuesta$IsTruncated)) break
    token <- as.character(respuesta$NextContinuationToken)
  }
  unique(keys)
}

listar_detalle_objetos_s3 <- function(
    cliente,
    prefijo_relativo = "",
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  prefijo <- key_absoluta_s3(prefijo_relativo, prefix)
  token <- NULL
  resultados <- list()

  repeat {
    argumentos <- list(Bucket = bucket, Prefix = prefijo)
    if (!is.null(token) && nzchar(token)) {
      argumentos$ContinuationToken <- token
    }
    respuesta <- do.call(cliente$list_objects_v2, argumentos)
    objetos <- respuesta$Contents
    if (!is.null(objetos) && length(objetos)) {
      resultados <- c(resultados, lapply(objetos, function(objeto) {
        data.frame(
          key = as.character(objeto$Key),
          bytes = as.numeric(objeto$Size),
          etag = gsub('^"|"$', "", as.character(objeto$ETag %||% "")),
          stringsAsFactors = FALSE
        )
      }))
    }
    if (!isTRUE(respuesta$IsTruncated)) break
    token <- as.character(respuesta$NextContinuationToken)
  }
  if (!length(resultados)) {
    return(data.frame(
      key = character(), bytes = numeric(), etag = character(),
      stringsAsFactors = FALSE
    ))
  }
  detalle <- do.call(rbind, resultados)
  detalle[!duplicated(detalle$key), , drop = FALSE]
}

`%||%` <- function(x, y) if (is.null(x)) y else x

planificar_descarga_prefijo_s3 <- function(
    cliente,
    prefijo_relativo,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  base <- key_absoluta_s3(prefijo_relativo, prefix)
  base_con_barra <- if (nzchar(base)) paste0(base, "/") else ""
  detalle <- listar_detalle_objetos_s3(
    cliente, base_con_barra, bucket = bucket, prefix = ""
  )
  detalle <- detalle[!endsWith(detalle$key, "/"), , drop = FALSE]
  if (!nrow(detalle)) {
    stop(sprintf("El prefijo S3 no contiene archivos: %s.", base),
      call. = FALSE)
  }
  relativas <- if (nzchar(base_con_barra)) {
    substring(detalle$key, nchar(base_con_barra) + 1L)
  } else {
    detalle$key
  }
  if (any(!nzchar(relativas)) || anyDuplicated(relativas) ||
      any(grepl("(^|/)[.][.](/|$)", relativas))) {
    stop("El prefijo S3 contiene claves relativas invalidas o duplicadas.",
      call. = FALSE)
  }
  data.frame(
    key = detalle$key,
    ruta_relativa = relativas,
    bytes = detalle$bytes,
    etag = detalle$etag,
    stringsAsFactors = FALSE
  )
}

validar_plan_descargado <- function(plan, directorio) {
  archivos <- vapply(
    plan$ruta_relativa, ruta_local_relativa, character(1L), raiz = directorio
  )
  existen <- file.exists(archivos) & !dir.exists(archivos)
  tamanios <- rep(NA_real_, length(archivos))
  tamanios[existen] <- file.info(archivos[existen])$size
  correctos <- existen & !is.na(tamanios) & tamanios == plan$bytes
  locales <- list.files(
    directorio, recursive = TRUE, full.names = TRUE, all.files = TRUE,
    no.. = TRUE
  )
  locales <- locales[!dir.exists(locales)]
  raiz <- normalizePath(directorio, winslash = "/", mustWork = TRUE)
  relativas_locales <- if (length(locales)) {
    substring(
      normalizePath(locales, winslash = "/", mustWork = TRUE),
      nchar(raiz) + 2L
    )
  } else {
    character()
  }
  inesperados <- setdiff(relativas_locales, plan$ruta_relativa)
  if (!all(correctos) || length(inesperados)) {
    detalle <- paste(c(
      plan$ruta_relativa[!correctos],
      if (length(inesperados)) paste0("inesperado:", inesperados)
    ), collapse = ", ")
    stop(sprintf("La descarga S3 quedo incompleta o con tamanio invalido: %s.",
      detalle), call. = FALSE)
  }
  invisible(archivos)
}

descargar_prefijo_s3 <- function(
    cliente,
    prefijo_relativo,
    destino_local,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE),
    reutilizar_existente = TRUE) {
  plan <- planificar_descarga_prefijo_s3(
    cliente, prefijo_relativo, bucket = bucket, prefix = prefix
  )
  if (dir.exists(destino_local)) {
    if (!isTRUE(reutilizar_existente)) {
      stop(sprintf("El destino local ya existe: %s.", destino_local),
        call. = FALSE)
    }
    validar_plan_descargado(plan, destino_local)
    return(list(destino = destino_local, plan = plan, reutilizado = TRUE))
  }
  if (file.exists(destino_local)) {
    stop(sprintf("El destino local existe y no es un directorio: %s.",
      destino_local), call. = FALSE)
  }
  padre <- dirname(destino_local)
  dir.create(padre, recursive = TRUE, showWarnings = FALSE)
  temporal <- tempfile(".descarga_s3_", tmpdir = padre)
  dir.create(temporal, recursive = TRUE, showWarnings = FALSE)
  promovido <- FALSE
  on.exit({
    if (!promovido && dir.exists(temporal)) {
      unlink(temporal, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  errores <- character()
  for (i in seq_len(nrow(plan))) {
    destino <- ruta_local_relativa(temporal, plan$ruta_relativa[[i]])
    dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
    tryCatch(
      cliente$download_file(bucket, plan$key[[i]], destino),
      error = function(error) {
        errores[[length(errores) + 1L]] <<- sprintf(
          "%s: %s", plan$key[[i]], conditionMessage(error)
        )
      }
    )
  }
  if (length(errores)) {
    stop(sprintf("Fallaron descargas S3: %s.", paste(errores, collapse = " | ")),
      call. = FALSE)
  }
  validar_plan_descargado(plan, temporal)
  if (!file.rename(temporal, destino_local)) {
    stop(sprintf("No se pudo promover la descarga al cache: %s.",
      destino_local), call. = FALSE)
  }
  promovido <- TRUE
  list(destino = destino_local, plan = plan, reutilizado = FALSE)
}

sincronizar_prefijo_s3 <- function(
    cliente,
    prefijo_relativo,
    destino_local,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  if (!dir.exists(destino_local)) {
    return(descargar_prefijo_s3(
      cliente, prefijo_relativo, destino_local,
      bucket = bucket, prefix = prefix, reutilizar_existente = FALSE
    ))
  }
  plan <- planificar_descarga_prefijo_s3(
    cliente, prefijo_relativo, bucket = bucket, prefix = prefix
  )
  vigente <- tryCatch({
    validar_plan_descargado(plan, destino_local)
    TRUE
  }, error = function(error) FALSE)
  if (vigente) {
    return(list(destino = destino_local, plan = plan, reutilizado = TRUE))
  }

  padre <- dirname(destino_local)
  nuevo <- tempfile(".sincronizacion_s3_", tmpdir = padre)
  descarga <- descargar_prefijo_s3(
    cliente, prefijo_relativo, nuevo,
    bucket = bucket, prefix = prefix, reutilizar_existente = FALSE
  )
  respaldo <- tempfile(".cache_anterior_", tmpdir = padre)
  if (!file.rename(destino_local, respaldo)) {
    unlink(nuevo, recursive = TRUE, force = TRUE)
    stop("No se pudo apartar el cache anterior para sincronizarlo.",
      call. = FALSE)
  }
  promovido <- file.rename(nuevo, destino_local)
  if (!promovido) {
    file.rename(respaldo, destino_local)
    unlink(nuevo, recursive = TRUE, force = TRUE)
    stop("No se pudo promover el prefijo S3 sincronizado.", call. = FALSE)
  }
  unlink(respaldo, recursive = TRUE, force = TRUE)
  descarga$destino <- destino_local
  descarga
}

leer_bytes_s3 <- function(
    cliente,
    ruta_relativa,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  key <- key_absoluta_s3(ruta_relativa, prefix)
  respuesta <- cliente$get_object(Bucket = bucket, Key = key)
  datos <- reticulate::py_to_r(respuesta$Body$read())
  if (is.raw(datos)) return(datos)
  as.raw(datos)
}

leer_texto_s3 <- function(
    cliente,
    ruta_relativa,
    encoding = "UTF-8",
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  datos <- leer_bytes_s3(
    cliente, ruta_relativa, bucket = bucket, prefix = prefix
  )
  iconv(rawToChar(datos), from = encoding, to = "UTF-8")
}

escribir_bytes_s3 <- function(
    cliente,
    ruta_relativa,
    datos,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  if (!is.raw(datos)) {
    stop("Los datos a escribir deben ser un vector raw.", call. = FALSE)
  }
  key <- key_absoluta_s3(ruta_relativa, prefix)
  cliente$put_object(
    Bucket = bucket,
    Key = key,
    Body = reticulate::r_to_py(datos)
  )
  key
}

escribir_texto_s3 <- function(
    cliente,
    ruta_relativa,
    texto,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  texto <- paste(enc2utf8(as.character(texto)), collapse = "\n")
  escribir_bytes_s3(
    cliente, ruta_relativa, charToRaw(texto),
    bucket = bucket, prefix = prefix
  )
}

descargar_archivo_s3 <- function(
    cliente,
    ruta_relativa,
    destino_local,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  key <- key_absoluta_s3(ruta_relativa, prefix)
  dir.create(dirname(destino_local), recursive = TRUE, showWarnings = FALSE)
  cliente$download_file(bucket, key, destino_local)
  destino_local
}

subir_archivo_s3 <- function(
    cliente,
    ruta_local,
    ruta_relativa,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE),
    sobrescribir = FALSE) {
  if (!file.exists(ruta_local) || dir.exists(ruta_local)) {
    stop(sprintf("No existe el archivo local: %s.", ruta_local),
      call. = FALSE)
  }
  key <- key_absoluta_s3(ruta_relativa, prefix)
  if (!isTRUE(sobrescribir) && existe_objeto_s3(
      cliente, ruta_relativa, bucket = bucket, prefix = prefix)) {
    stop(sprintf("El objeto S3 ya existe y no sera sobrescrito: %s.", key),
      call. = FALSE)
  }
  cliente$upload_file(ruta_local, bucket, key)
  key
}

obtener_metadatos_objeto_s3 <- function(
    cliente,
    ruta_relativa,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  key <- key_absoluta_s3(ruta_relativa, prefix)
  respuesta <- cliente$head_object(Bucket = bucket, Key = key)
  list(
    key = key,
    bytes = as.numeric(respuesta$ContentLength),
    etag = gsub('^"|"$', "", as.character(respuesta$ETag %||% ""))
  )
}

objeto_s3_identico_archivo <- function(
    cliente,
    ruta_local,
    ruta_relativa,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  metadatos <- obtener_metadatos_objeto_s3(
    cliente, ruta_relativa, bucket = bucket, prefix = prefix
  )
  if (!identical(as.numeric(file.info(ruta_local)$size), metadatos$bytes)) {
    return(FALSE)
  }
  temporal <- tempfile("comparacion_s3_")
  on.exit(unlink(temporal, force = TRUE), add = TRUE)
  descargar_archivo_s3(
    cliente, ruta_relativa, temporal, bucket = bucket, prefix = prefix
  )
  identical(
    unname(tools::md5sum(ruta_local)), unname(tools::md5sum(temporal))
  )
}

planificar_subida_directorio_s3 <- function(
    directorio_local,
    prefijo_relativo = "",
    incluir_ocultos = FALSE,
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  raiz <- normalizePath(directorio_local, winslash = "/", mustWork = TRUE)
  archivos <- list.files(
    raiz, recursive = TRUE, full.names = TRUE, all.files = incluir_ocultos,
    no.. = TRUE
  )
  archivos <- archivos[!dir.exists(archivos)]
  if (!isTRUE(incluir_ocultos)) {
    relativas_previas <- substring(
      normalizePath(archivos, winslash = "/", mustWork = TRUE),
      nchar(raiz) + 2L
    )
    archivos <- archivos[!grepl("(^|/)[.]", relativas_previas)]
  }
  if (!length(archivos)) {
    stop(sprintf("El directorio no contiene archivos para subir: %s.", raiz),
      call. = FALSE)
  }
  normalizados <- normalizePath(archivos, winslash = "/", mustWork = TRUE)
  relativas <- substring(normalizados, nchar(raiz) + 2L)
  rutas_objeto <- vapply(
    relativas, function(x) unir_key_s3(prefijo_relativo, x), character(1L)
  )
  data.frame(
    local = archivos,
    ruta_relativa = rutas_objeto,
    key = vapply(rutas_objeto, key_absoluta_s3, character(1L), prefix = prefix),
    bytes = as.numeric(file.info(archivos)$size),
    stringsAsFactors = FALSE
  )
}

subir_directorio_s3 <- function(
    cliente,
    directorio_local,
    prefijo_relativo = "",
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE),
    permitir_existentes_identicos = FALSE,
    rutas_al_final = character()) {
  plan <- planificar_subida_directorio_s3(
    directorio_local, prefijo_relativo, prefix = prefix
  )
  al_final <- plan$ruta_relativa %in% rutas_al_final
  plan <- plan[order(al_final, plan$ruta_relativa), , drop = FALSE]
  plan$estado <- "pendiente"

  for (i in seq_len(nrow(plan))) {
    existe <- existe_objeto_s3(
      cliente, plan$ruta_relativa[[i]], bucket = bucket, prefix = prefix
    )
    if (existe) {
      identico <- isTRUE(permitir_existentes_identicos) &&
        objeto_s3_identico_archivo(
          cliente, plan$local[[i]], plan$ruta_relativa[[i]],
          bucket = bucket, prefix = prefix
        )
      if (!identico) {
        stop(sprintf("El objeto S3 ya existe y difiere o no puede reutilizarse: %s.",
          plan$key[[i]]), call. = FALSE)
      }
      plan$estado[[i]] <- "reutilizado"
    }
  }
  for (i in which(plan$estado == "pendiente")) {
    subir_archivo_s3(
      cliente, plan$local[[i]], plan$ruta_relativa[[i]],
      bucket = bucket, prefix = prefix, sobrescribir = FALSE
    )
    remoto <- obtener_metadatos_objeto_s3(
      cliente, plan$ruta_relativa[[i]], bucket = bucket, prefix = prefix
    )
    if (!identical(remoto$bytes, plan$bytes[[i]])) {
      stop(sprintf("El objeto subido tiene tamanio inesperado: %s.",
        plan$key[[i]]), call. = FALSE)
    }
    plan$estado[[i]] <- "subido"
  }
  plan
}

existe_objeto_s3 <- function(
    cliente,
    ruta_relativa,
    bucket = leer_variable_s3("ASIGNA_FLUJOS_S3_BUCKET"),
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  key <- key_absoluta_s3(ruta_relativa, prefix)
  tryCatch(
    {
      cliente$head_object(Bucket = bucket, Key = key)
      TRUE
    },
    error = function(error) {
      mensaje <- conditionMessage(error)
      if (grepl("404|Not Found|NoSuchKey", mensaje, ignore.case = TRUE)) {
        return(FALSE)
      }
      stop(error)
    }
  )
}
