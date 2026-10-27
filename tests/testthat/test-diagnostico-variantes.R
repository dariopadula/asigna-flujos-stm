source(file.path("..", "..", "pipeline_diario", "R", "variantes", "diagnosticar_variantes.R"))

crear_insumos_diagnostico <- function(raiz) {
  od <- data.frame(
    sevar_codigo_subida = c(100L, 200L, 300L),
    sevar_codigo_bajada = c(100L, 200L, 300L),
    linea_codigo_subida = c(10L, 20L, 30L),
    linea_codigo_bajada = c(10L, 20L, 30L),
    cantidad_pasajeros = c(1L, 2L, 1L)
  )
  archivo_od <- file.path(raiz, "od_operativa.parquet")
  arrow::write_parquet(od, archivo_od)

  geometrias <- data.frame(
    codigo = c(100L, 100L, 300L),
    sentido = c("A", "A", "B"),
    fecha_vigencia = c("2025-01-01 00:00:00", "2026-01-01 00:00:00", "2026-01-01 00:00:00"),
    fecha_baja = c("2025-12-31 00:00:00", "", ""),
    nombre_variante = c("Vieja", "Variante 100", "Variante 300"),
    the_geom = c(
      "LINESTRING (0 0, 1 1)",
      "LINESTRING (0 0, 1 1)",
      "LINESTRING (1 0, 2 1)"
    )
  )
  archivo_geometrias <- file.path(raiz, "variantes.csv")
  data.table::fwrite(geometrias, archivo_geometrias)

  columnas_paradas <- c("cod_ubic_parada", "cod_variante", "ordinal", "cod_linea", "anio_mes", "x", "y")
  paradas0 <- data.frame(
    cod_ubic_parada = c(1L, 2L), cod_variante = c(100L, 100L),
    ordinal = c(0L, 1L), cod_linea = c(10L, 10L), anio_mes = c(202605L, 202605L),
    x = c(-56.1, -56.2), y = c(-34.8, -34.9)
  )
  paradas1 <- data.frame(
    cod_ubic_parada = c(3L, 4L), cod_variante = c(200L, 200L),
    ordinal = c(0L, 1L), cod_linea = c(20L, 20L), anio_mes = c(202605L, 202605L),
    x = c(-56.3, -56.4), y = c(-34.7, -34.6)
  )
  paradas0 <- paradas0[columnas_paradas]
  paradas1 <- paradas1[columnas_paradas]
  archivos_paradas <- file.path(raiz, c("paradas0.csv", "paradas1.csv"))
  data.table::fwrite(paradas0, archivos_paradas[[1]])
  data.table::fwrite(paradas1, archivos_paradas[[2]])

  catalogo <- data.frame(
    lineas_metro.codigo = c(10L, 20L, 30L),
    lineas_metro.nombre_publico = c("10", "20", "30"),
    lineas_metro.cod_empresa = c(1L, 2L, 3L),
    lineas_metro.codigo_subsistema = c(1L, 2L, 2L),
    lineas_metro.anio = rep(2026L, 3L),
    lineas_metro.anio_mes = rep(202605L, 3L),
    check.names = FALSE
  )
  archivo_catalogo <- file.path(raiz, "lineas_metro.csv")
  data.table::fwrite(catalogo, archivo_catalogo)

  list(
    od = archivo_od,
    geometrias = archivo_geometrias,
    paradas = archivos_paradas,
    catalogo = archivo_catalogo
  )
}

test_that("el diagnostico separa variantes completas y referencias faltantes", {
  raiz <- tempfile("diagnostico_variantes_")
  dir.create(raiz)
  insumos <- crear_insumos_diagnostico(raiz)
  resultado <- diagnosticar_variantes(
    "2026-05-20", insumos$od, insumos$geometrias, insumos$paradas,
    insumos$catalogo, file.path(raiz, "salida")
  )

  estados <- resultado$diagnostico[, setNames(estado_diagnostico, codigo_variante)]
  expect_equal(estados[["100"]], "completa")
  expect_equal(estados[["200"]], "sin_geometria_vigente")
  expect_equal(estados[["300"]], "sin_paradas_mes")
  expect_equal(resultado$resumen[estado_diagnostico == "TOTAL", variantes], 3)
  expect_true(file.exists(file.path(resultado$destino, "diagnostico_variantes.csv")))
  expect_true(file.exists(file.path(resultado$destino, "variantes_con_problemas.csv")))
})

test_that("el diagnostico no sobrescribe una salida existente", {
  raiz <- tempfile("diagnostico_existente_")
  dir.create(raiz)
  insumos <- crear_insumos_diagnostico(raiz)
  destino <- file.path(raiz, "salida", "diagnostico_variantes", "fecha=2026-05-20")
  dir.create(destino, recursive = TRUE)

  expect_error(
    diagnosticar_variantes(
      "2026-05-20", insumos$od, insumos$geometrias, insumos$paradas,
      insumos$catalogo, file.path(raiz, "salida")
    ),
    "ya existe"
  )
})

