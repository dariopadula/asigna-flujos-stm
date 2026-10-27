source(file.path(
  "..", "..", "pipeline_referencias", "R", "variantes",
  "preparar_universo_variantes.R"
))

test_that("el universo de referencias se construye sin OD", {
  raiz <- tempfile("universo_referencias_")
  dir.create(raiz)
  crear_paradas <- function(codigos, offset = 0L) {
    data.table::data.table(
      cod_ubic_parada = seq_along(codigos) + offset,
      desc_ubic_parada = paste("P", seq_along(codigos) + offset),
      cod_nombre_via_1 = 1L,
      cod_nombre_via_2 = 2L,
      nom_calle = "CALLE",
      cod_variante = codigos,
      ordinal = ave(seq_along(codigos), codigos, FUN = seq_along),
      cod_linea = codigos,
      anio_mes = 202605L,
      x = seq_along(codigos),
      y = seq_along(codigos) + 10
    )
  }
  parte0 <- crear_paradas(c(100L, 100L, 200L, 200L, 300L, 300L, 400L, 500L))
  parte1 <- parte0[1]
  archivos_paradas <- file.path(raiz, c("paradas0.csv", "paradas1.csv"))
  data.table::fwrite(parte0, archivos_paradas[[1]])
  data.table::fwrite(parte1, archivos_paradas[[2]])

  lineas <- data.table::data.table(
    lineas_metro.codigo = c(100L, 200L, 300L, 400L, 500L),
    lineas_metro.nombre_publico = paste0("L", c(100L, 200L, 300L, 400L, 500L)),
    lineas_metro.cod_empresa = 1L,
    lineas_metro.codigo_subsistema = c(1L, 1L, 2L, 9L, 1L),
    lineas_metro.anio_mes = 202605L
  )
  archivo_lineas <- file.path(raiz, "lineas.csv")
  data.table::fwrite(lineas, archivo_lineas)

  geometrias <- data.table::data.table(
    codigo = c(100L, 200L, 300L, 300L, 500L),
    sentido = "A",
    subli_codigo = 1L,
    fecha_vigencia = c(
      "2026-01-01", "2026-01-01", "2025-01-01", "2026-02-01", "2026-01-01"
    ),
    fecha_baja = c("", "", "2026-01-31", "", "2026-05-01"),
    nombre_variante = paste0("V", c(100L, 200L, 300L, 300L, 500L)),
    the_geom = c(
      "LINESTRING (0 0, 1 1)", "", "LINESTRING (0 0, 1 1)",
      "LINESTRING (0 0, 2 2)", "LINESTRING (0 0, 3 3)"
    )
  )
  archivo_geometrias <- file.path(raiz, "geometrias.csv")
  data.table::fwrite(geometrias, archivo_geometrias)

  resultado <- preparar_universo_variantes(
    "prueba", "2026-05-20", 202605L, c(1L, 2L),
    archivo_geometrias, archivos_paradas, archivo_lineas,
    file.path(raiz, "salida"), 2L, 4L
  )
  expect_setequal(resultado$universo$codigo_variante, c("100", "300"))
  expect_equal(
    resultado$universo[codigo_variante == "300", id_version_variante],
    "300_20260201"
  )
  expect_equal(resultado$pendientes[codigo == "200", estado_referencia], "sin_wkt")
  expect_equal(resultado$pendientes[codigo == "500", estado_referencia], "dada_de_baja")
  expect_true(file.exists(file.path(resultado$destino, "paradas_universo.parquet")))
  expect_error(
    preparar_universo_variantes(
      "prueba", "2026-05-20", 202605L, c(1L, 2L),
      archivo_geometrias, archivos_paradas, archivo_lineas,
      file.path(raiz, "salida")
    ),
    "ya existe"
  )
})
