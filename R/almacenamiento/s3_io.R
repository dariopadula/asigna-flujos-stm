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
  partes <- gsub("^/+|/+$", "", partes)
  partes <- partes[nzchar(partes)]
  paste(partes, collapse = "/")
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
    prefix = leer_variable_s3("ASIGNA_FLUJOS_S3_PREFIX", obligatoria = FALSE)) {
  if (!file.exists(ruta_local) || dir.exists(ruta_local)) {
    stop(sprintf("No existe el archivo local: %s.", ruta_local),
      call. = FALSE)
  }
  key <- key_absoluta_s3(ruta_relativa, prefix)
  cliente$upload_file(ruta_local, bucket, key)
  key
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
