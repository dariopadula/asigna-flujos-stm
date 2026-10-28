crear_cliente_s3_simulado <- function(raiz) {
  dir.create(raiz, recursive = TRUE, showWarnings = FALSE)

  ruta_key <- function(bucket, key) {
    partes <- strsplit(gsub("\\\\", "/", key), "/", fixed = TRUE)[[1L]]
    do.call(file.path, c(list(raiz, bucket), as.list(partes)))
  }
  listar <- function(bucket, prefix) {
    base <- file.path(raiz, bucket)
    if (!dir.exists(base)) return(character())
    archivos <- list.files(base, recursive = TRUE, full.names = TRUE)
    archivos <- archivos[!dir.exists(archivos)]
    if (!length(archivos)) return(character())
    claves <- substring(
      normalizePath(archivos, winslash = "/", mustWork = TRUE),
      nchar(normalizePath(base, winslash = "/", mustWork = TRUE)) + 2L
    )
    claves[startsWith(claves, prefix)]
  }

  cliente <- new.env(parent = emptyenv())
  cliente$.uploads <- character()
  cliente$list_objects_v2 <- function(Bucket, Prefix = "", ContinuationToken = NULL) {
    claves <- listar(Bucket, Prefix)
    contenidos <- lapply(claves, function(key) {
      archivo <- ruta_key(Bucket, key)
      list(
        Key = key,
        Size = as.numeric(file.info(archivo)$size),
        ETag = unname(tools::md5sum(archivo))
      )
    })
    list(Contents = contenidos, IsTruncated = FALSE)
  }
  cliente$head_object <- function(Bucket, Key) {
    archivo <- ruta_key(Bucket, Key)
    if (!file.exists(archivo)) stop("404 Not Found")
    list(
      ContentLength = as.numeric(file.info(archivo)$size),
      ETag = unname(tools::md5sum(archivo))
    )
  }
  cliente$upload_file <- function(Filename, Bucket, Key) {
    destino <- ruta_key(Bucket, Key)
    dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(Filename, destino, overwrite = TRUE)) {
      stop("No se pudo simular la subida.")
    }
    cliente$.uploads <- c(cliente$.uploads, Key)
    invisible(NULL)
  }
  cliente$download_file <- function(Bucket, Key, Filename) {
    origen <- ruta_key(Bucket, Key)
    if (!file.exists(origen)) stop("404 Not Found")
    dir.create(dirname(Filename), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(origen, Filename, overwrite = TRUE)) {
      stop("No se pudo simular la descarga.")
    }
    invisible(NULL)
  }
  cliente$.ruta_key <- ruta_key
  cliente
}
