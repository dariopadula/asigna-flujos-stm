materializar_directorio_persistente <- function(
    ubicacion,
    destino_cache,
    cliente_s3 = NULL) {
  if (!grepl("^s3[a-z0-9+.-]*://", ubicacion)) {
    if (!dir.exists(ubicacion)) {
      stop(sprintf("No existe el directorio persistente: %s.", ubicacion),
        call. = FALSE)
    }
    return(normalizePath(ubicacion, winslash = "/", mustWork = TRUE))
  }
  if (is.null(cliente_s3)) {
    stop("Se requiere un cliente S3 para materializar el directorio.",
      call. = FALSE)
  }
  partes <- descomponer_uri_s3(ubicacion)
  descargar_prefijo_s3(
    cliente = cliente_s3,
    prefijo_relativo = "",
    destino_local = destino_cache,
    bucket = partes$bucket,
    prefix = partes$prefix,
    reutilizar_existente = TRUE
  )
  normalizePath(destino_cache, winslash = "/", mustWork = TRUE)
}

metadatos_referencias_equivalentes <- function(local, remoto) {
  nombre <- basename(local)
  if (identical(nombre, "manifiesto_archivos.csv")) {
    columnas <- c("archivo", "bytes", "md5")
    esperado <- data.table::fread(local)[, ..columnas]
    observado <- data.table::fread(remoto)[, ..columnas]
    esperado[, `:=`(
      archivo = as.character(archivo), bytes = as.numeric(bytes),
      md5 = as.character(md5)
    )]
    observado[, `:=`(
      archivo = as.character(archivo), bytes = as.numeric(bytes),
      md5 = as.character(md5)
    )]
    data.table::setorder(esperado, archivo)
    data.table::setorder(observado, archivo)
    return(identical(esperado, observado))
  }
  if (identical(nombre, "manifiesto.yml")) {
    campos <- c("tipo", "red_canonica", "version_regiones")
    esperado <- yaml::read_yaml(local)
    observado <- yaml::read_yaml(remoto)
    return(identical(esperado[campos], observado[campos]))
  }
  FALSE
}

subir_referencias_estaticas_s3 <- function(
    cliente,
    directorio_local,
    bucket,
    prefix) {
  archivos <- list.files(
    directorio_local, recursive = TRUE, full.names = TRUE
  )
  archivos <- archivos[!dir.exists(archivos)]
  es_metadato <- basename(archivos) %in% c(
    "manifiesto_archivos.csv", "manifiesto.yml"
  )
  plan_datos <- subir_directorio_s3(
    cliente,
    directorio_local = directorio_local,
    prefijo_relativo = "referencias",
    bucket = bucket,
    prefix = prefix,
    permitir_existentes_identicos = TRUE,
    archivos_locales = archivos[!es_metadato]
  )
  plan_metadatos <- planificar_subida_directorio_s3(
    directorio_local,
    prefijo_relativo = "referencias",
    prefix = prefix,
    archivos_locales = archivos[es_metadato]
  )
  plan_metadatos$estado <- "pendiente"
  for (i in seq_len(nrow(plan_metadatos))) {
    ruta <- plan_metadatos$ruta_relativa[[i]]
    if (existe_objeto_s3(
        cliente, ruta, bucket = bucket, prefix = prefix)) {
      temporal <- tempfile("referencia_s3_")
      on.exit(unlink(temporal, force = TRUE), add = TRUE)
      descargar_archivo_s3(
        cliente, ruta, temporal, bucket = bucket, prefix = prefix
      )
      if (!metadatos_referencias_equivalentes(
          plan_metadatos$local[[i]], temporal)) {
        stop(sprintf(
          "Los metadatos de referencias S3 existentes no son equivalentes: %s.",
          plan_metadatos$key[[i]]
        ), call. = FALSE)
      }
      unlink(temporal, force = TRUE)
      plan_metadatos$estado[[i]] <- "reutilizado"
    } else {
      subir_archivo_s3(
        cliente, plan_metadatos$local[[i]], ruta,
        bucket = bucket, prefix = prefix
      )
      remoto <- obtener_metadatos_objeto_s3(
        cliente, ruta, bucket = bucket, prefix = prefix
      )
      if (!identical(remoto$bytes, plan_metadatos$bytes[[i]])) {
        stop(sprintf(
          "El metadato de referencias subido tiene tamanio inesperado: %s.",
          plan_metadatos$key[[i]]
        ), call. = FALSE)
      }
      plan_metadatos$estado[[i]] <- "subido"
    }
  }
  rbind(plan_datos, plan_metadatos)
}

publicar_dia_almacenamiento <- function(
    fecha,
    directorio_ocupacion,
    directorio_insumos,
    directorio_regiones,
    directorio_red_canonica,
    published_root,
    backend,
    work_root,
    cliente_s3 = NULL) {
  if (identical(backend, "local")) {
    return(publicar_dia(
      fecha = fecha,
      directorio_ocupacion = directorio_ocupacion,
      directorio_insumos = directorio_insumos,
      directorio_regiones = directorio_regiones,
      directorio_red_canonica = directorio_red_canonica,
      directorio_publicado = published_root
    ))
  }
  if (!identical(backend, "s3")) {
    stop(sprintf("Backend de publicacion desconocido: %s.", backend),
      call. = FALSE)
  }
  if (is.null(cliente_s3)) {
    stop("Se requiere un cliente S3 para publicar.", call. = FALSE)
  }
  ubicacion <- descomponer_uri_s3(published_root)
  catalogo_relativo <- unir_key_s3(
    "catalogo", paste0("fecha=", fecha, ".yml")
  )
  if (existe_objeto_s3(
      cliente_s3, catalogo_relativo,
      bucket = ubicacion$bucket, prefix = ubicacion$prefix)) {
    stop(sprintf("El catalogo S3 ya contiene la fecha %s.", fecha),
      call. = FALSE)
  }

  dir.create(work_root, recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile("publicacion_", tmpdir = work_root)
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  resultado <- publicar_dia(
    fecha = fecha,
    directorio_ocupacion = directorio_ocupacion,
    directorio_insumos = directorio_insumos,
    directorio_regiones = directorio_regiones,
    directorio_red_canonica = directorio_red_canonica,
    directorio_publicado = staging
  )

  plan_referencias <- subir_referencias_estaticas_s3(
    cliente_s3,
    directorio_local = file.path(staging, "referencias"),
    bucket = ubicacion$bucket,
    prefix = ubicacion$prefix
  )
  relativo_dia <- unir_key_s3("dias", paste0("fecha=", fecha))
  plan_dia <- subir_directorio_s3(
    cliente_s3,
    directorio_local = file.path(staging, "dias", paste0("fecha=", fecha)),
    prefijo_relativo = relativo_dia,
    bucket = ubicacion$bucket,
    prefix = ubicacion$prefix,
    permitir_existentes_identicos = TRUE
  )
  archivo_catalogo <- file.path(
    staging, "catalogo", paste0("fecha=", fecha, ".yml")
  )
  key_catalogo <- subir_archivo_s3(
    cliente_s3,
    ruta_local = archivo_catalogo,
    ruta_relativa = catalogo_relativo,
    bucket = ubicacion$bucket,
    prefix = ubicacion$prefix,
    sobrescribir = FALSE
  )
  metadatos_catalogo <- obtener_metadatos_objeto_s3(
    cliente_s3, catalogo_relativo,
    bucket = ubicacion$bucket, prefix = ubicacion$prefix
  )
  if (!identical(
      metadatos_catalogo$bytes,
      as.numeric(file.info(archivo_catalogo)$size))) {
    stop("El catalogo S3 no supero la verificacion de tamanio.",
      call. = FALSE)
  }
  resultado$destino <- paste0(
    sub("/+$", "", published_root), "/", relativo_dia
  )
  resultado$archivo_catalogo <- key_catalogo
  resultado$transferencia <- list(
    referencias = plan_referencias,
    dia = plan_dia,
    catalogo = key_catalogo
  )
  resultado
}
