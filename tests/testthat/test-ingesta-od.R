raiz_ingesta <- file.path("..", "..")
source(file.path(raiz_ingesta, "R", "almacenamiento", "s3_io.R"))
source(file.path(
  raiz_ingesta, "pipeline_diario", "R", "ingesta", "ingerir_od_dia.R"
))
source(file.path(
  raiz_ingesta, "pipeline_diario", "R", "ingesta", "materializar_od_dia.R"
))

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
  manifiesto <- yaml::read_yaml(file.path(resultado$destino, "manifiesto.yml"))
  expect_identical(manifiesto$tipo, "od_normalizada_diaria")
  expect_equal(manifiesto$salida$filas, 2)
  expect_true(length(manifiesto$esquema) >= ncol(muestra))
  expect_true(length(manifiesto$salida$archivos) >= 3L)

  datos <- arrow::open_dataset(
    list.files(resultado$destino, pattern = "[.]parquet$", full.names = TRUE),
    format = "parquet"
  )
  expect_equal(dplyr::collect(dplyr::summarise(datos, filas = dplyr::n()))$filas, 2)

  reconstruido <- ingerir_od_dia(
    fecha = "2026-05-20",
    archivo_od = archivo_od,
    directorio_salida = salida,
    columnas_requeridas = names(muestra),
    columnas_clave = c("codigo_parada_subida", "codigo_parada_bajada"),
    sobrescribir = TRUE
  )
  expect_equal(as.numeric(reconstruido$resumen$filas), 2)
})

test_that("una apertura mensual produce fechas aisladas", {
  raiz <- tempfile("ingesta_mes_")
  dir.create(raiz)
  archivo <- file.path(raiz, "od.csv")
  muestra <- data.frame(
    fecha_evento_subida = c(
      "2026-05-20 08:00:00", "2026-05-21 09:00:00"
    ),
    fecha_evento_bajada = c(
      "2026-05-20 08:10:00", "2026-05-21 09:10:00"
    ),
    linea_codigo_subida = 1:2,
    sevar_codigo_subida = 10:11,
    codigo_parada_subida = 100:101,
    codigo_parada_bajada = 200:201,
    cantidad_pasajeros = c(2L, 3L),
    id_recorrido_global = c("a", "b"),
    valid = "True", global = "True", anio_mes = 202605L
  )
  data.table::fwrite(muestra, archivo)

  resultados <- normalizar_od_mensual(
    archivo_od = archivo,
    directorio_salida = file.path(raiz, "ingested"),
    columnas_requeridas = names(muestra),
    columnas_clave = c("codigo_parada_subida", "codigo_parada_bajada"),
    fechas = c("2026-05-20", "2026-05-21")
  )
  expect_length(resultados, 2L)
  expect_equal(vapply(
    resultados, function(x) as.numeric(x$resumen$filas), numeric(1L)
  ), c(1, 1))
  expect_equal(vapply(
    resultados, function(x) as.numeric(x$resumen$personas), numeric(1L)
  ), c(2, 3))
})

test_that("la normalizacion rechaza un esquema incompleto", {
  raiz <- tempfile("ingesta_esquema_")
  dir.create(raiz)
  archivo <- file.path(raiz, "od.csv")
  data.table::fwrite(data.frame(
    fecha_evento_subida = "2026-05-20 08:00:00"
  ), archivo)
  expect_error(
    normalizar_od_mensual(
      archivo_od = archivo,
      directorio_salida = file.path(raiz, "ingested"),
      columnas_requeridas = c("fecha_evento_subida", "cantidad_pasajeros"),
      columnas_clave = "fecha_evento_subida",
      fechas = "2026-05-20"
    ),
    "Faltan columnas"
  )
})

test_that("la particion se materializa igual desde local y S3 simulado", {
  raiz <- tempfile("materializar_ingesta_")
  dir.create(raiz)
  archivo <- file.path(raiz, "od.csv")
  muestra <- data.frame(
    fecha_evento_subida = "2026-05-20 08:00:00",
    fecha_evento_bajada = "2026-05-20 08:10:00",
    linea_codigo_subida = 1L, sevar_codigo_subida = 10L,
    codigo_parada_subida = 100L, codigo_parada_bajada = 200L,
    cantidad_pasajeros = 2L, id_recorrido_global = "a",
    valid = "True", global = "True", anio_mes = 202605L
  )
  data.table::fwrite(muestra, archivo)
  ingested <- file.path(raiz, "ingested")
  resultado <- ingerir_od_dia(
    "2026-05-20", archivo, ingested, names(muestra),
    c("codigo_parada_subida", "codigo_parada_bajada")
  )
  local <- materializar_od_dia(
    "2026-05-20", ingested, "local", file.path(raiz, "cache"),
    columnas_requeridas = names(muestra)
  )
  expect_identical(local, normalizePath(
    resultado$destino, winslash = "/", mustWork = TRUE
  ))

  cliente <- crear_cliente_s3_simulado(file.path(raiz, "s3"))
  subir_od_dia_s3(
    "2026-05-20", resultado$destino,
    "s3://bucket/base/ingested", cliente,
    columnas_requeridas = names(muestra)
  )
  remoto <- materializar_od_dia(
    "2026-05-20", "s3://bucket/base/ingested", "s3",
    file.path(raiz, "cache"), columnas_requeridas = names(muestra),
    cliente_s3 = cliente
  )
  expect_true(file.exists(file.path(remoto, "manifiesto.yml")))
  expect_equal(
    as.numeric(yaml::read_yaml(file.path(remoto, "manifiesto.yml"))$salida$filas),
    1
  )
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

