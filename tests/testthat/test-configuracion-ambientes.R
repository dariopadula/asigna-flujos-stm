raiz_config <- file.path("..", "..")
source(file.path(raiz_config, "config", "cargar_configuracion.R"))
source(file.path(raiz_config, "R", "almacenamiento", "s3_io.R"))
source(file.path(
  raiz_config, "R", "almacenamiento", "materializar_configuracion.R"
))

testthat::test_that("el perfil local separa y resuelve todas las raices", {
  configuracion <- cargar_configuracion(raiz_config, "local")

  testthat::expect_identical(configuracion$environment, "local")
  testthat::expect_identical(configuracion$storage$backend, "local")
  testthat::expect_match(
    configuracion$storage$ingested_root, "data/ingested$"
  )
  testthat::expect_true(all(vapply(
    configuracion$storage[names(configuracion$storage) != "backend"],
    es_ruta_absoluta, logical(1L)
  )))
  testthat::expect_true(es_ruta_absoluta(configuracion$execution$work_root))
  testthat::expect_true(es_ruta_absoluta(configuracion$execution$cache_root))
  testthat::expect_match(
    resolver_fuente_od(configuracion, "2026-05-21"),
    "stm_od_202605.csv$"
  )
  testthat::expect_identical(
    configuracion$publication$output_root,
    configuracion$storage$published_root
  )
})

testthat::test_that("el perfil cloudera usa S3 persistente y disco del job", {
  variables <- c(
    ASIGNA_FLUJOS_SOURCE_ROOT = "s3://ejemplo/source",
    ASIGNA_FLUJOS_INGESTED_ROOT = "s3://ejemplo/ingested",
    ASIGNA_FLUJOS_REFERENCE_ROOT = "s3://ejemplo/reference",
    ASIGNA_FLUJOS_REGISTRY_ROOT = "s3://ejemplo/registry",
    ASIGNA_FLUJOS_PUBLISHED_ROOT = "s3://ejemplo/published",
    ASIGNA_FLUJOS_QUARANTINE_ROOT = "s3://ejemplo/quarantine",
    ASIGNA_FLUJOS_WORK_ROOT = file.path(tempdir(), "work"),
    ASIGNA_FLUJOS_CACHE_ROOT = file.path(tempdir(), "cache")
  )
  nombres <- names(variables)
  anteriores <- Sys.getenv(nombres, unset = NA_character_)
  on.exit({
    Sys.unsetenv(nombres[is.na(anteriores)])
    conservar <- !is.na(anteriores)
    if (any(conservar)) {
      do.call(Sys.setenv, as.list(stats::setNames(
        anteriores[conservar], nombres[conservar]
      )))
    }
  }, add = TRUE)
  do.call(Sys.setenv, as.list(variables))

  configuracion <- cargar_configuracion(raiz_config, "cloudera")
  persistentes <- configuracion$storage[
    names(configuracion$storage) != "backend"
  ]
  testthat::expect_true(all(vapply(persistentes, es_uri_objeto, logical(1L))))
  testthat::expect_false(es_uri_objeto(configuracion$execution$work_root))
  testthat::expect_identical(
    resolver_fuente_od(configuracion, "2026-05-21"),
    "s3://ejemplo/source/stm_od_202605.csv"
  )
  testthat::expect_identical(
    configuracion$storage$ingested_root, "s3://ejemplo/ingested"
  )
})

testthat::test_that("la configuracion falla ante perfiles o variables invalidas", {
  testthat::expect_error(
    cargar_configuracion(raiz_config, "inexistente"),
    "Perfil desconocido"
  )
  variable <- "ASIGNA_FLUJOS_SOURCE_ROOT"
  anterior <- Sys.getenv(variable, unset = NA_character_)
  on.exit({
    if (is.na(anterior)) Sys.unsetenv(variable) else Sys.setenv(
      ASIGNA_FLUJOS_SOURCE_ROOT = anterior
    )
  }, add = TRUE)
  Sys.unsetenv(variable)
  testthat::expect_error(
    cargar_configuracion(raiz_config, "cloudera"),
    "ASIGNA_FLUJOS_SOURCE_ROOT"
  )
  invalida <- cargar_configuracion(raiz_config, "local")
  invalida$storage$backend <- "desconocido"
  testthat::expect_error(
    validar_configuracion_ambiente(invalida), "Backend de almacenamiento"
  )
})

testthat::test_that("referencias y registros S3 se materializan en cache", {
  raiz <- tempfile("config_s3_")
  dir.create(raiz)
  cliente <- crear_cliente_s3_simulado(file.path(raiz, "s3"))
  referencia <- file.path(raiz, "referencia")
  registro <- file.path(raiz, "registro")
  dir.create(referencia)
  dir.create(registro)
  writeLines("catalogo", file.path(referencia, "lineas_metro.csv"))
  writeLines("registro", file.path(registro, "manifiesto.yml"))
  subir_directorio_s3(
    cliente, referencia, bucket = "bucket", prefix = "base/reference"
  )
  subir_directorio_s3(
    cliente, registro, bucket = "bucket", prefix = "base/registry"
  )
  configuracion <- cargar_configuracion(raiz_config, "local")
  configuracion$storage$backend <- "s3"
  configuracion$storage$reference_root <- "s3://bucket/base/reference"
  configuracion$storage$registry_root <- "s3://bucket/base/registry"
  configuracion$execution$cache_root <- file.path(raiz, "cache")

  materializada <- materializar_configuracion_lectura(
    configuracion, cliente, c("reference_root", "registry_root")
  )
  testthat::expect_true(file.exists(materializada$paths$line_catalog))
  testthat::expect_true(file.exists(file.path(
    materializada$storage$registry_root, "manifiesto.yml"
  )))
  testthat::expect_false(es_uri_objeto(
    materializada$storage$reference_root
  ))
  testthat::expect_identical(materializada$storage$backend, "s3")
})
