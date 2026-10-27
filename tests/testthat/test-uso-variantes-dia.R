source(file.path(
  "..", "..", "pipeline_diario", "R", "variantes",
  "resolver_uso_variantes_dia.R"
))

crear_entradas_uso_variantes <- function(raiz, duplicar_registro = FALSE) {
  od <- data.table::data.table(
    sevar_codigo_subida = c("100", "100", "200", "300"),
    linea_codigo_subida = c("10", "10", "20", "30"),
    numero_evento_real_subida = c("S1", "S1", "S2", "S3"),
    sevar_codigo_bajada = c("100", "200", "400", "300"),
    linea_codigo_bajada = c("10", "20", "40", "30"),
    numero_evento_real_bajada = c("B1", "B2", "B4", "B3"),
    cantidad_pasajeros = c(1, 2, 3, 4)
  )
  archivo_od <- file.path(raiz, "od_operativa.parquet")
  arrow::write_parquet(od, archivo_od)

  registro <- data.table::data.table(
    codigo_variante = c("100", "200", "300"),
    id_version_variante = c("100_20260101", "200_20260101", "300_20260101"),
    codigo_linea = c("10", "20", "30"),
    codigo_empresa = c("1", "2", "3"),
    codigo_subsistema = c(1L, 2L, 4L),
    sentido = c("A", "A", "B"),
    fecha_vigencia = "2026-01-01",
    fecha_baja = NA_character_
  )
  if (duplicar_registro) registro <- rbind(registro, registro[1L])
  directorio_registro <- file.path(raiz, "registro")
  dir.create(directorio_registro)
  arrow::write_parquet(
    registro, file.path(directorio_registro, "variantes.parquet")
  )
  yaml::write_yaml(list(
    etapa = "registro_canonico",
    estado = "completo",
    version = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1"
  ), file.path(directorio_registro, "manifiesto.yml"))
  list(od = archivo_od, registro = directorio_registro)
}

testthat::test_that("el uso diario referencia el registro sin duplicar objetos canonicos", {
  raiz <- tempfile("uso_variantes_")
  dir.create(raiz)
  entradas <- crear_entradas_uso_variantes(raiz)
  resultado <- resolver_uso_variantes_dia(
    "2026-05-20", entradas$od, entradas$registro,
    file.path(raiz, "intermediate")
  )

  testthat::expect_equal(nrow(resultado$uso), 4L)
  testthat::expect_equal(resultado$uso[estado_resolucion == "resuelta", .N], 3L)
  testthat::expect_equal(resultado$no_resueltas$codigo_variante, "400")
  testthat::expect_true(all(resultado$controles$aprobado))
  testthat::expect_equal(sum(resultado$uso$registros_subida), 4L)
  testthat::expect_equal(sum(resultado$uso$registros_bajada), 4L)
  testthat::expect_equal(sum(resultado$uso$personas_subida), 10)
  testthat::expect_equal(sum(resultado$uso$personas_bajada), 10)
  testthat::expect_false(any(c(
    "geometria_wkt", "punto_canonico_id", "indice_secuencia"
  ) %in% names(resultado$uso)))
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "uso_variantes.parquet"
  )))
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "variantes_no_resueltas.csv"
  )))
  testthat::expect_error(
    resolver_uso_variantes_dia(
      "2026-05-20", entradas$od, entradas$registro,
      file.path(raiz, "intermediate")
    ),
    "ya existe"
  )
})

testthat::test_that("el resolvedor rechaza codigos duplicados en el registro", {
  raiz <- tempfile("uso_variantes_duplicadas_")
  dir.create(raiz)
  entradas <- crear_entradas_uso_variantes(raiz, duplicar_registro = TRUE)
  testthat::expect_error(
    resolver_uso_variantes_dia(
      "2026-05-20", entradas$od, entradas$registro,
      file.path(raiz, "intermediate")
    ),
    "mas de una version por codigo"
  )
})
