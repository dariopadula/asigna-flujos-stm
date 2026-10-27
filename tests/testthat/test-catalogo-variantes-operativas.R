source(file.path(
  "..", "..", "pipeline_diario", "R", "variantes",
  "preparar_catalogo_variantes_operativas.R"
))

test_that("el catalogo separa variantes listas y pendientes sin inferir geometria", {
  raiz <- tempfile("catalogo_variantes_")
  dir.create(raiz)
  diagnostico <- data.table::data.table(
    codigo_variante = c("100", "200", "300"),
    geometria_estado = c("vigente_valida", "sin_wkt", "vigente_valida"),
    fecha_vigencia = c("2026-01-01", "2026-01-01", "2026-02-01"),
    registros_subida = c(10L, 3L, 5L),
    registros_bajada = c(10L, 3L, 5L)
  )
  resumen_paradas <- data.table::data.table(
    codigo_variante = c("100", "200", "300"),
    filas_normalizadas = c(2L, 2L, 2L),
    paradas_distintas = c(2L, 2L, 2L),
    duplicados_exactos_eliminados = 0L,
    filas_sin_ordinal_ancla = 0L,
    ordinales_positivos_compartidos = c(0L, 0L, 1L),
    filas_sin_coordenadas = 0L,
    estado_normalizacion = c(
      "lista_para_proyeccion", "lista_para_proyeccion",
      "lista_con_ordinal_compartido"
    )
  )
  paradas <- data.table::data.table(
    codigo_variante = rep(c("100", "200", "300"), each = 2L),
    cod_ubic_parada = 1:6,
    ordinal_ancla = rep(1:2, 3),
    x = 1:6,
    y = 11:16
  )
  geometrias <- data.table::data.table(
    codigo = c(100L, 200L, 300L),
    sentido = c("A", "A", "B"),
    fecha_vigencia = c(
      "2026-01-01 00:00:00", "2026-01-01 00:00:00",
      "2026-02-01 00:00:00"
    ),
    fecha_baja = c("", "", ""),
    nombre_variante = c("V100", "V200", "V300"),
    the_geom = c("LINESTRING (0 0, 1 1)", "", "LINESTRING (0 0, 2 2)")
  )

  archivo_diagnostico <- file.path(raiz, "diagnostico.csv")
  archivo_resumen <- file.path(raiz, "resumen.csv")
  archivo_paradas <- file.path(raiz, "paradas.parquet")
  archivo_geometrias <- file.path(raiz, "geometrias.csv")
  data.table::fwrite(diagnostico, archivo_diagnostico)
  data.table::fwrite(resumen_paradas, archivo_resumen)
  arrow::write_parquet(paradas, archivo_paradas)
  data.table::fwrite(geometrias, archivo_geometrias)

  resultado <- preparar_catalogo_variantes_operativas(
    "2026-05-20", archivo_diagnostico, archivo_geometrias,
    archivo_paradas, archivo_resumen, file.path(raiz, "salida")
  )

  expect_equal(resultado$catalogo[lista_para_proyeccion == TRUE, .N], 2L)
  expect_equal(resultado$pendientes$codigo_variante, "200")
  expect_equal(
    resultado$catalogo[codigo_variante == "100", id_version_variante],
    "100_20260101"
  )
  expect_true(resultado$catalogo[codigo_variante == "300", requiere_resolucion_espacial])
  paradas_catalogo <- data.table::as.data.table(arrow::read_parquet(file.path(
    resultado$destino, "paradas_variantes_catalogo.parquet"
  )))
  expect_equal(nrow(paradas_catalogo), 4L)
  expect_false("200" %in% paradas_catalogo$codigo_variante)
  expect_error(
    preparar_catalogo_variantes_operativas(
      "2026-05-20", archivo_diagnostico, archivo_geometrias,
      archivo_paradas, archivo_resumen, file.path(raiz, "salida")
    ),
    "ya existe"
  )
})
