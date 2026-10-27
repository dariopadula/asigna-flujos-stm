source(file.path("..", "..", "pipeline_diario", "R", "ingesta", "ingerir_od_dia.R"))

test_that("la ingesta conserva solo el dia solicitado y genera controles", {
  raiz_temporal <- tempfile("prueba_ingesta_")
  dir.create(raiz_temporal)
  archivo_od <- file.path(raiz_temporal, "od.csv")
  salida <- file.path(raiz_temporal, "intermediate")

  muestra <- data.frame(
    fecha_evento_subida = c(
      "2026-05-19 23:59:59", "2026-05-20 00:00:00",
      "2026-05-20 12:30:00", "2026-05-21 00:00:00"
    ),
    fecha_evento_bajada = c(
      "2026-05-20 00:10:00", "2026-05-20 00:15:00",
      "2026-05-20 12:45:00", "2026-05-21 00:15:00"
    ),
    linea_codigo_subida = c(1L, 1L, 2L, 2L),
    sevar_codigo_subida = c(10L, 10L, 20L, 20L),
    codigo_parada_subida = c(100L, 100L, 200L, 200L),
    codigo_parada_bajada = c(101L, 101L, NA_integer_, 201L),
    cantidad_pasajeros = c(1L, 1L, 2L, 1L),
    id_recorrido_global = c("a", "b", "c", "d"),
    valid = c("True", "True", "True", "True"),
    global = c("True", "True", "True", "True"),
    anio_mes = rep(202605L, 4L)
  )
  data.table::fwrite(muestra, archivo_od)

  resultado <- ingerir_od_dia(
    fecha = "2026-05-20",
    archivo_od = archivo_od,
    directorio_salida = salida,
    columnas_requeridas = names(muestra),
    columnas_clave = c("codigo_parada_subida", "codigo_parada_bajada")
  )

  expect_equal(as.numeric(resultado$resumen$filas), 2)
  expect_equal(as.numeric(resultado$resumen$filas_global), 2)
  expect_equal(as.numeric(resultado$resumen$personas_global), 3)
  expect_equal(as.numeric(resultado$resumen$variantes_subida), 2)
  expect_equal(
    resultado$faltantes[columna == "codigo_parada_bajada", faltantes],
    1
  )
  expect_true(file.exists(file.path(resultado$destino, "resumen_ingesta.csv")))
  expect_true(file.exists(file.path(resultado$destino, "manifiesto.yml")))

  datos <- arrow::open_dataset(
    list.files(resultado$destino, pattern = "[.]parquet$", full.names = TRUE),
    format = "parquet"
  )
  expect_equal(dplyr::collect(dplyr::summarise(datos, filas = dplyr::n()))$filas, 2)
})

test_that("la ingesta rechaza una fecha ya generada", {
  raiz_temporal <- tempfile("prueba_ingesta_existente_")
  archivo_od <- file.path(raiz_temporal, "od.csv")
  destino <- file.path(raiz_temporal, "od", "fecha=2026-05-20")
  dir.create(destino, recursive = TRUE)
  data.table::fwrite(data.frame(fecha_evento_subida = "2026-05-20 00:00:00"), archivo_od)

  expect_error(
    ingerir_od_dia(
      fecha = "2026-05-20",
      archivo_od = archivo_od,
      directorio_salida = raiz_temporal,
      columnas_requeridas = "fecha_evento_subida",
      columnas_clave = "fecha_evento_subida"
    ),
    "ya existe"
  )
})

test_that("las descripciones de linea aceptan valores alfanumericos", {
  raiz_temporal <- tempfile("prueba_ingesta_texto_")
  dir.create(raiz_temporal)
  archivo_od <- file.path(raiz_temporal, "od.csv")

  muestra <- data.frame(
    fecha_evento_subida = c("2026-05-20 01:00:00", "2026-05-20 02:00:00"),
    fecha_evento_bajada = c("2026-05-20 01:10:00", "2026-05-20 02:10:00"),
    linea_codigo_subida = c(48L, 48L),
    sevar_codigo_subida = c(10L, 10L),
    codigo_parada_subida = c(100L, 101L),
    codigo_parada_bajada = c(101L, 102L),
    cantidad_pasajeros = c(1L, 1L),
    id_recorrido_global = c("a", "b"),
    desc_linea_subida = c("48", "48D"),
    desc_linea_bajada = c("48", "48D"),
    valid = c("True", "True"),
    global = c("True", "True"),
    anio_mes = c(202605L, 202605L)
  )
  data.table::fwrite(muestra, archivo_od)

  fuente <- abrir_od_csv(archivo_od)
  datos <- dplyr::collect(dplyr::select(fuente, desc_linea_subida))

  expect_equal(datos$desc_linea_subida, c("48", "48D"))
})

