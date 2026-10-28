raiz_s3_io <- file.path("..", "..")
source(file.path(raiz_s3_io, "R", "almacenamiento", "s3_io.R"))

testthat::test_that("las claves S3 se normalizan sin barras duplicadas", {
  testthat::expect_identical(
    unir_key_s3("/base/", "/published/", "archivo.parquet"),
    "base/published/archivo.parquet"
  )
  testthat::expect_identical(unir_key_s3("", "/archivo.txt"), "archivo.txt")
  testthat::expect_identical(unir_key_s3(), "")
})

testthat::test_that("las URI S3 separan bucket y prefijo", {
  testthat::expect_identical(
    descomponer_uri_s3("s3://bucket/base/data/ingested/"),
    list(bucket = "bucket", prefix = "base/data/ingested")
  )
  testthat::expect_identical(
    descomponer_uri_s3("s3://bucket"),
    list(bucket = "bucket", prefix = "")
  )
  testthat::expect_error(descomponer_uri_s3("data/ingested"), "URI S3")
})

testthat::test_that("la configuracion S3 se obtiene del ambiente", {
  nombres <- c(
    "ASIGNA_FLUJOS_S3_CONNECTION",
    "ASIGNA_FLUJOS_S3_BUCKET",
    "ASIGNA_FLUJOS_S3_PREFIX"
  )
  anteriores <- Sys.getenv(nombres, unset = NA_character_)
  on.exit({
    Sys.unsetenv(nombres)
    conservar <- !is.na(anteriores)
    if (any(conservar)) {
      do.call(Sys.setenv, as.list(stats::setNames(
        anteriores[conservar], nombres[conservar]
      )))
    }
  }, add = TRUE)
  Sys.setenv(
    ASIGNA_FLUJOS_S3_CONNECTION = "conexion-prueba",
    ASIGNA_FLUJOS_S3_BUCKET = "bucket-prueba",
    ASIGNA_FLUJOS_S3_PREFIX = "/base/data/"
  )

  configuracion <- obtener_configuracion_s3()
  testthat::expect_identical(configuracion$connection_name, "conexion-prueba")
  testthat::expect_identical(configuracion$bucket, "bucket-prueba")
  testthat::expect_identical(configuracion$prefix, "/base/data/")
  testthat::expect_identical(
    key_absoluta_s3("published/dia.parquet"),
    "base/data/published/dia.parquet"
  )
})

testthat::test_that("faltan variables S3 obligatorias de forma explicita", {
  variable <- "ASIGNA_FLUJOS_S3_BUCKET"
  anterior <- Sys.getenv(variable, unset = NA_character_)
  on.exit({
    if (is.na(anterior)) {
      Sys.unsetenv(variable)
    } else {
      Sys.setenv(ASIGNA_FLUJOS_S3_BUCKET = anterior)
    }
  }, add = TRUE)
  Sys.unsetenv(variable)

  testthat::expect_error(
    leer_variable_s3(variable),
    "ASIGNA_FLUJOS_S3_BUCKET"
  )
})

testthat::test_that("un directorio se sube y descarga con verificacion", {
  raiz <- tempfile("s3_simulado_")
  cliente <- crear_cliente_s3_simulado(file.path(raiz, "objetos"))
  local <- file.path(raiz, "local")
  dir.create(file.path(local, "sub"), recursive = TRUE)
  writeLines("uno", file.path(local, "a.txt"))
  writeLines("dos", file.path(local, "sub", "b.txt"))

  subida <- subir_directorio_s3(
    cliente, local, "od/fecha=2026-05-20",
    bucket = "prueba", prefix = "base/ingested"
  )
  testthat::expect_true(all(subida$estado == "subido"))
  testthat::expect_setequal(
    listar_objetos_s3(
      cliente, "od/fecha=2026-05-20/",
      bucket = "prueba", prefix = "base/ingested"
    ),
    c(
      "base/ingested/od/fecha=2026-05-20/a.txt",
      "base/ingested/od/fecha=2026-05-20/sub/b.txt"
    )
  )
  testthat::expect_error(
    subir_directorio_s3(
      cliente, local, "od/fecha=2026-05-20",
      bucket = "prueba", prefix = "base/ingested"
    ),
    "ya existe"
  )

  destino <- file.path(raiz, "cache", "fecha=2026-05-20")
  descarga <- descargar_prefijo_s3(
    cliente, "od/fecha=2026-05-20", destino,
    bucket = "prueba", prefix = "base/ingested"
  )
  testthat::expect_false(descarga$reutilizado)
  testthat::expect_identical(readLines(file.path(destino, "a.txt")), "uno")
  testthat::expect_identical(
    readLines(file.path(destino, "sub", "b.txt")), "dos"
  )
  repetida <- descargar_prefijo_s3(
    cliente, "od/fecha=2026-05-20", destino,
    bucket = "prueba", prefix = "base/ingested"
  )
  testthat::expect_true(repetida$reutilizado)
})
