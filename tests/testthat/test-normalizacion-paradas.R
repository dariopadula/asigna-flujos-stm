source(file.path("..", "..", "pipeline_diario", "R", "variantes", "normalizar_paradas_variantes.R"))

test_that("la normalizacion replica los criterios de paradas vigentes", {
  raiz <- tempfile("normalizacion_paradas_")
  dir.create(raiz)
  diagnostico <- file.path(raiz, "diagnostico.csv")
  data.table::fwrite(data.frame(codigo_variante = c("100", "200")), diagnostico)

  crear_paradas <- function(variante, parada, ordinal, linea, x, y) {
    data.frame(
      cod_ubic_parada = parada,
      desc_ubic_parada = paste("Parada", parada),
      cod_nombre_via_1 = 1L,
      cod_nombre_via_2 = 2L,
      nom_calle = "CALLE",
      cod_variante = variante,
      ordinal = ordinal,
      cod_linea = linea,
      anio_mes = 202605L,
      x = x,
      y = y
    )
  }
  parte0 <- crear_paradas(
    c(100L, 100L, 100L, 100L, 200L),
    c(1L, 2L, 3L, 4L, 5L),
    c(0L, 1L, 1L, 3L, 5L),
    c(10L, 10L, 10L, 10L, 20L),
    c(-56.1, -56.2, -56.3, -56.4, -56.5),
    c(-34.1, -34.2, -34.3, -34.4, -34.5)
  )
  parte1 <- parte0[1, ]
  archivos <- file.path(raiz, c("paradas0.csv", "paradas1.csv"))
  data.table::fwrite(parte0, archivos[[1]])
  data.table::fwrite(parte1, archivos[[2]])

  resultado <- normalizar_paradas_variantes(
    "2026-05-20", diagnostico, archivos, file.path(raiz, "salida")
  )
  normalizadas <- data.table::as.data.table(arrow::read_parquet(
    file.path(resultado$destino, "paradas_normalizadas.parquet")
  ))

  expect_equal(nrow(normalizadas), 5)
  expect_equal(sum(normalizadas$repeticiones_fuente - 1L), 1)
  expect_true(is.na(normalizadas[codigo_variante == "100" & cod_ubic_parada == 1L, ordinal_ancla]))
  expect_equal(normalizadas[codigo_variante == "100" & ordinal_positivo_compartido, .N], 2)
  expect_true(3L %in% normalizadas[codigo_variante == "100", ordinal_ancla])
  expect_equal(
    resultado$resumen_variantes[codigo_variante == "100", estado_normalizacion],
    "lista_con_ordinal_compartido"
  )
})

test_that("la normalizacion no sobrescribe una corrida", {
  raiz <- tempfile("normalizacion_existente_")
  dir.create(file.path(raiz, "salida", "paradas_variantes_normalizadas", "fecha=2026-05-20"), recursive = TRUE)
  diagnostico <- file.path(raiz, "diagnostico.csv")
  parada <- file.path(raiz, "paradas.csv")
  data.table::fwrite(data.frame(codigo_variante = "100"), diagnostico)
  data.table::fwrite(data.frame(x = 1), parada)

  expect_error(
    normalizar_paradas_variantes(
      "2026-05-20", diagnostico, parada, file.path(raiz, "salida")
    ),
    "ya existe"
  )
})

