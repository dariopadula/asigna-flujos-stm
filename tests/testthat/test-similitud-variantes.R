source(file.path("..", "..", "pipeline_diario", "R", "variantes", "comparar_variantes_misma_linea.R"))

test_that("la similitud exige cobertura, cercania y orden", {
  expect_equal(clasificar_similitud(0.8, 0.95, 100, 200), "alta")
  expect_equal(clasificar_similitud(0.4, 0.50, 250, 400), "parcial")
  expect_equal(clasificar_similitud(0.2, 1.00, 50, 50), "baja")
})

