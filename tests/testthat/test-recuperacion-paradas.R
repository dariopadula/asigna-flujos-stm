source(file.path(
  "..", "..", "pipeline_diario", "R", "paradas",
  "recuperar_paradas_variantes_distintas.R"
))

crear_entradas_recuperacion_paradas <- function(
    raiz, alterar_snapshot = FALSE) {
  fecha <- "2026-05-20"
  od <- data.table::data.table(
    trip_id = 1:7,
    linea_codigo_subida = rep("10", 7),
    linea_codigo_bajada = c("10", "10", "10", "20", "10", "10", "10"),
    cantidad_pasajeros = 1:7,
    tipo_servicio = "urbano"
  )
  archivo_od <- file.path(raiz, "od_operativa.parquet")
  arrow::write_parquet(od, archivo_od)

  directorio_paso09 <- file.path(raiz, "paso09")
  dir.create(directorio_paso09)
  resolucion <- data.table::data.table(
    trip_id = 1:7,
    codigo_variante_subida = c("A", "A", "A", "A", "Z", "A", "A"),
    codigo_variante_bajada = c("B", "B", "B", "B", "B", "B", "Z"),
    codigo_parada_subida = c(
      "O", "O", "O", "O", "O", "NO_EXISTE", "O"
    ),
    codigo_parada_bajada = c("D", "DB", "FAR", "DB", "DB", "DB", "DB"),
    estado_resolucion = "variante_subida_bajada_distinta"
  )
  archivo_paso09 <- file.path(directorio_paso09, "resolucion_paradas.parquet")
  arrow::write_parquet(resolucion, archivo_paso09)
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1"
  ), file.path(directorio_paso09, "manifiesto.yml"))

  directorio_uso <- file.path(raiz, "uso")
  dir.create(directorio_uso)
  uso <- data.table::data.table(
    codigo_variante = c("A", "B", "Z"),
    id_version_variante = c("A_1", "B_1", NA_character_),
    estado_resolucion = c("resuelta", "resuelta", "no_resuelta_registro")
  )
  arrow::write_parquet(uso, file.path(directorio_uso, "uso_variantes.parquet"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = if (alterar_snapshot) {
      "otro_snapshot"
    } else {
      "referencias_prueba"
    },
    red_canonica = "canonica_prueba_v1"
  ), file.path(directorio_uso, "manifiesto.yml"))

  directorio_registro <- file.path(raiz, "registro")
  dir.create(directorio_registro)
  paradas <- data.table::data.table(
    id_version_variante = c("A_1", "A_1", "A_1", "B_1", "B_1", "B_1"),
    cod_ubic_parada = c("O", "D", "C", "D", "DB", "FAR"),
    indice_secuencia = c(1L, 5L, 6L, 2L, 3L, 4L),
    punto_canonico_id = c("A1", "A5", "A6", "B2", "B3", "B4"),
    x = c(-56, -55.999, -55.998, -55.999, -55.9979, -55.95),
    y = -34
  )
  arrow::write_parquet(
    paradas, file.path(directorio_registro, "paradas_proyectadas.parquet")
  )
  yaml::write_yaml(list(
    etapa = "registro_canonico",
    estado = "completo",
    version = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1"
  ), file.path(directorio_registro, "manifiesto.yml"))

  list(
    fecha = fecha,
    od = archivo_od,
    paso09 = archivo_paso09,
    uso = directorio_uso,
    registro = directorio_registro
  )
}

testthat::test_that("la recuperacion genera solo un delta auditable", {
  raiz <- tempfile("recuperacion_paradas_")
  dir.create(raiz)
  entradas <- crear_entradas_recuperacion_paradas(raiz)
  resultado <- recuperar_paradas_variantes_distintas(
    entradas$fecha, entradas$od, entradas$paso09, entradas$uso,
    entradas$registro, file.path(raiz, "intermediate"),
    distancia_maxima_m = 500
  )

  delta <- resultado$recuperaciones
  testthat::expect_equal(nrow(delta), 7L)
  testthat::expect_true(all(resultado$controles$aprobado))
  testthat::expect_equal(
    delta[trip_id == 1L, accion_resolucion],
    "parada_exacta_en_variante_subida"
  )
  testthat::expect_equal(
    delta[trip_id == 1L, codigo_parada_bajada_asignada], "D"
  )
  testthat::expect_equal(
    delta[trip_id == 2L, accion_resolucion],
    "parada_cercana_en_variante_subida"
  )
  testthat::expect_equal(
    delta[trip_id == 2L, codigo_parada_bajada_asignada], "C"
  )
  testthat::expect_true(delta[trip_id == 2L, distancia_recuperacion_m] <= 500)
  testthat::expect_true(delta[trip_id == 2L, requiere_revision])
  testthat::expect_equal(
    delta[trip_id == 3L, accion_resolucion], "distancia_mayor_umbral"
  )
  testthat::expect_false(delta[trip_id == 3L, recuperada])
  testthat::expect_equal(
    delta[trip_id == 4L, accion_resolucion], "linea_distinta"
  )
  testthat::expect_equal(
    delta[trip_id == 5L, accion_resolucion], "variante_subida_no_resuelta"
  )
  testthat::expect_equal(
    delta[trip_id == 6L, accion_resolucion], "origen_no_resuelto"
  )
  testthat::expect_equal(
    delta[trip_id == 7L, accion_resolucion], "variante_bajada_no_resuelta"
  )
  testthat::expect_false(any(c(
    "cantidad_pasajeros", "tipo_servicio", "x", "y", "geometry"
  ) %in% names(delta)))
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "recuperaciones_paradas.parquet"
  )))
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "resumen_recuperacion.csv"
  )))
  testthat::expect_error(
    recuperar_paradas_variantes_distintas(
      entradas$fecha, entradas$od, entradas$paso09, entradas$uso,
      entradas$registro, file.path(raiz, "intermediate"), 500
    ),
    "ya existe"
  )
})

testthat::test_that("la recuperacion exige la misma entrega canonica", {
  raiz <- tempfile("recuperacion_paradas_snapshot_")
  dir.create(raiz)
  entradas <- crear_entradas_recuperacion_paradas(
    raiz, alterar_snapshot = TRUE
  )
  testthat::expect_error(
    recuperar_paradas_variantes_distintas(
      entradas$fecha, entradas$od, entradas$paso09, entradas$uso,
      entradas$registro, file.path(raiz, "intermediate"), 500
    ),
    "misma entrega"
  )
})

testthat::test_that("la distancia maxima debe ser positiva", {
  raiz <- tempfile("recuperacion_paradas_umbral_")
  dir.create(raiz)
  entradas <- crear_entradas_recuperacion_paradas(raiz)
  testthat::expect_error(
    recuperar_paradas_variantes_distintas(
      entradas$fecha, entradas$od, entradas$paso09, entradas$uso,
      entradas$registro, file.path(raiz, "intermediate"), 0
    ),
    "numero positivo"
  )
})
