source(file.path("..", "..", "pipeline_diario", "R", "ingesta", "preparar_od_operativa.R"))

crear_muestra_operativa <- function(raiz) {
  entrada <- file.path(raiz, "od", "fecha=2026-05-20")
  dir.create(entrada, recursive = TRUE)
  od <- data.frame(
    numero_evento_subida = 1:3,
    linea_codigo_subida = c(10L, 20L, 10L),
    sevar_codigo_subida = c(100L, 200L, 100L),
    nro_frecuencia_subida = 1:3,
    numero_tarjeta_mifare = c("a", "b", "c"),
    secuencia_multitramo = 1:3,
    ordinal_de_tramo = c(1L, 1L, 1L),
    cantidad_pasajeros = c(1L, 2L, 5L),
    fecha_evento_subida = as.POSIXct(c("2026-05-20 01:00:00", "2026-05-20 02:00:00", "2026-05-20 03:00:00"), tz = "UTC"),
    codigo_parada_subida = c(1L, 2L, 3L),
    numero_evento_real_subida = c("r1", "r2", "r3"),
    fecha_evento_bajada = c("2026-05-20 01:10:00", "2026-05-20 02:10:00", "2026-05-20 03:10:00"),
    codigo_parada_bajada = c(2L, 3L, 4L),
    numero_evento_real_bajada = c("r1", "r2", "r3"),
    linea_codigo_bajada = c(10L, 20L, 10L),
    sevar_codigo_bajada = c(100L, 200L, 100L),
    nro_frecuencia_bajada = 1:3,
    valid = c("True", "True", "True"),
    cod_empresa = c(1L, 2L, 1L),
    id_recorrido_global = c("r1", "r2", "r3"),
    extrapolation = c("False", "False", "False"),
    global = c("True", "TRUE", "False")
  )
  arrow::write_parquet(od, file.path(entrada, "od-0.parquet"))

  catalogo <- data.frame(
    lineas_metro.codigo = c(10L, 20L),
    lineas_metro.nombre_publico = c("10", "20"),
    lineas_metro.cod_empresa = c(1L, 2L),
    lineas_metro.codigo_subsistema = c(1L, 2L),
    lineas_metro.anio = c(2026L, 2026L),
    lineas_metro.anio_mes = c(202605L, 202605L),
    check.names = FALSE
  )
  catalogo_path <- file.path(raiz, "lineas_metro.csv")
  data.table::fwrite(catalogo, catalogo_path)
  list(entrada = file.path(raiz, "od"), catalogo = catalogo_path, columnas = names(od))
}

test_that("la preparacion filtra globales y clasifica subsistemas", {
  raiz <- tempfile("od_operativa_")
  dir.create(raiz)
  muestra <- crear_muestra_operativa(raiz)
  resultado <- preparar_od_operativa(
    "2026-05-20", muestra$entrada, muestra$catalogo,
    file.path(raiz, "salida"), muestra$columnas
  )

  total <- resultado$resumen[tipo_servicio == "total"]
  expect_equal(total$registros, 2)
  expect_equal(total$personas, 3)
  expect_equal(sort(resultado$resumen[tipo_servicio != "total", tipo_servicio]), c("inter", "urbano"))
  expect_true(file.exists(file.path(resultado$destino, "od_operativa.parquet")))
})

test_that("la preparacion rechaza lineas sin catalogo", {
  raiz <- tempfile("od_sin_catalogo_")
  dir.create(raiz)
  muestra <- crear_muestra_operativa(raiz)
  catalogo <- data.table::fread(muestra$catalogo)[1]
  data.table::fwrite(catalogo, muestra$catalogo)

  expect_error(
    preparar_od_operativa(
      "2026-05-20", muestra$entrada, muestra$catalogo,
      file.path(raiz, "salida"), muestra$columnas
    ),
    "sin catalogo"
  )
})

