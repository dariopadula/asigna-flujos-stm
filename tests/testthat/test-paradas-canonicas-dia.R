source(file.path(
  "..", "..", "pipeline_diario", "R", "paradas",
  "resolver_paradas_canonicas_dia.R"
))

crear_entradas_paradas_dia <- function(raiz, alterar_snapshot = FALSE) {
  od <- data.table::data.table(
    trip_id = 1:8,
    sevar_codigo_subida = c("A", "A", "A", "A", "A", "A", "Z", "A"),
    sevar_codigo_bajada = c("A", "A", "A", "A", "A", "B", "Z", "A"),
    codigo_parada_subida = c("O", "X", "Q", "F", "O", "O", "O", "F1"),
    codigo_parada_bajada = c("D", "D", "Q", "D", "F", "D", "D", "F2"),
    cantidad_pasajeros = 1:8,
    tipo_servicio = "urbano"
  )
  archivo_od <- file.path(raiz, "od_operativa.parquet")
  arrow::write_parquet(od, archivo_od)

  directorio_uso <- file.path(raiz, "uso")
  dir.create(directorio_uso)
  uso <- data.table::data.table(
    codigo_variante = c("A", "B", "Z"),
    id_version_variante = c("A_1", "B_1", NA_character_),
    estado_resolucion = c("resuelta", "resuelta", "no_resuelta_registro")
  )
  arrow::write_parquet(uso, file.path(directorio_uso, "uso_variantes.parquet"))
  yaml::write_yaml(list(
    fecha_operativa = "2026-05-20",
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = if (alterar_snapshot) "otro_snapshot" else "referencias_prueba",
    red_canonica = "canonica_prueba_v1"
  ), file.path(directorio_uso, "manifiesto.yml"))

  directorio_registro <- file.path(raiz, "registro")
  dir.create(directorio_registro)
  paradas <- data.table::data.table(
    id_version_variante = c(
      "A_1", "A_1", "A_1", "A_1", "A_1", "B_1", "B_1"
    ),
    cod_ubic_parada = c("O", "X", "X", "Q", "D", "O", "D"),
    indice_secuencia = c(1L, 2L, 8L, 3L, 5L, 1L, 5L),
    punto_canonico_id = c("P1", "P2", "P8", "P3", "P5", "B1", "B5"),
    estado_calidad = "aceptada",
    metodo_asignacion = "espacial",
    distancia_asignada_m = 1
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
    od = archivo_od,
    uso = directorio_uso,
    registro = directorio_registro
  )
}

testthat::test_that("las paradas diarias se resuelven sin copiar secuencias", {
  raiz <- tempfile("paradas_dia_")
  dir.create(raiz)
  entradas <- crear_entradas_paradas_dia(raiz)
  resultado <- resolver_paradas_canonicas_dia(
    "2026-05-20", entradas$od, entradas$uso, entradas$registro,
    file.path(raiz, "intermediate")
  )

  testthat::expect_equal(nrow(resultado$resolucion), 8L)
  testthat::expect_true(all(resultado$controles$aprobado))
  testthat::expect_equal(
    resultado$resolucion[trip_id == 1L, estado_resolucion], "apta_secuencia"
  )
  testthat::expect_equal(
    resultado$resolucion[trip_id == 2L, indice_secuencia_subida], 2L
  )
  testthat::expect_equal(
    resultado$resolucion[trip_id == 2L, combinaciones_candidatas], 2L
  )
  testthat::expect_equal(
    resultado$resolucion[trip_id == 3L, estado_resolucion], "misma_posicion"
  )
  testthat::expect_equal(
    resultado$resolucion[trip_id == 4L, estado_resolucion], "sin_parada_subida"
  )
  testthat::expect_equal(
    resultado$resolucion[trip_id == 5L, estado_resolucion], "sin_parada_bajada"
  )
  testthat::expect_equal(
    resultado$resolucion[trip_id == 6L, estado_resolucion],
    "variante_subida_bajada_distinta"
  )
  testthat::expect_equal(
    resultado$resolucion[trip_id == 7L, estado_resolucion],
    "sin_variante_canonica"
  )
  testthat::expect_equal(
    resultado$resolucion[trip_id == 8L, estado_resolucion],
    "sin_ambas_paradas"
  )
  testthat::expect_false(any(c(
    "x", "y", "geometry", "secuencia_completa"
  ) %in% names(resultado$resolucion)))
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "resolucion_paradas.parquet"
  )))
  testthat::expect_error(
    resolver_paradas_canonicas_dia(
      "2026-05-20", entradas$od, entradas$uso, entradas$registro,
      file.path(raiz, "intermediate")
    ),
    "ya existe"
  )
})

testthat::test_that("el resolvedor exige la misma entrega canonica", {
  raiz <- tempfile("paradas_dia_snapshot_")
  dir.create(raiz)
  entradas <- crear_entradas_paradas_dia(raiz, alterar_snapshot = TRUE)
  testthat::expect_error(
    resolver_paradas_canonicas_dia(
      "2026-05-20", entradas$od, entradas$uso, entradas$registro,
      file.path(raiz, "intermediate")
    ),
    "misma entrega"
  )
})
