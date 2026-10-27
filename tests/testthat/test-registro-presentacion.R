source(file.path(
  "..", "..", "pipeline_referencias", "R", "presentacion",
  "construir_registro_presentacion.R"
))
source(file.path(
  "..", "..", "pipeline_referencias", "R", "presentacion",
  "publicar_registro_presentacion.R"
))

crear_fuentes_presentacion_prueba <- function(raiz) {
  puntos <- sf::st_as_sf(data.frame(
    canonical_point_id = paste0("P", 1:5),
    corridor_id = c("MVD_1", "MVD_1", "MVD_2354", "MVD_2354", "EXT_1"),
    corridor_key = c("MVD_1", "MVD_1", "MVD_2354", "MVD_2354", "EXT_1"),
    corridor_point_index = c(1L, 2L, 1L, 2L, 1L),
    osm_ref = NA_character_,
    osm_name = c("Calle Norte", "Calle Norte", "Avenida", "Avenida", NA),
    network_zone = c(rep("montevideo", 4), "exterior"),
    X = c(0, 0, 10, 20, 30), Y = c(10, 0, 0, 0, 0)
  ), coords = c("X", "Y"), crs = 32721)
  referencia <- sf::st_as_sf(data.frame(
    COD_NOMBRE = c(1L, 2354L),
    dirIni = c("NORTE", "OESTE"), dirFin = c("SUR", "ESTE"),
    X = c(0, 10), Y = c(0, 0)
  ), coords = c("X", "Y"), crs = 32721)
  secuencias <- data.table::data.table(
    codigo_variante = c(rep("A", 2), rep("B", 2), "C"),
    id_version_variante = c(rep("A_1", 2), rep("B_1", 2), "C_1"),
    codigo_linea = c(rep("L1", 2), rep("L2", 2), "L3"),
    nombre_linea = c(rep("1", 2), rep("2", 2), "3"),
    sentido = c(rep("A", 2), rep("B", 2), "A"),
    indice_secuencia = c(1:2, 1:2, 1L),
    punto_canonico_id = paste0("P", 1:5),
    corredor_id = c("MVD_1", "MVD_1", "MVD_2354", "MVD_2354", "EXT_1"),
    indice_punto_corredor = c(1L, 2L, 1L, 2L, 1L),
    x_canonica = c(0, 0, 10, 20, 30), y_canonica = c(10, 0, 0, 0, 0)
  )
  rutas <- list(
    secuencias = file.path(raiz, "secuencias.parquet"),
    puntos = file.path(raiz, "catalogo_puntos.rds"),
    referencia = file.path(raiz, "referencia_urbana.rds"),
    registro = file.path(raiz, "registro")
  )
  arrow::write_parquet(secuencias, rutas$secuencias)
  saveRDS(puntos, rutas$puntos)
  saveRDS(referencia, rutas$referencia)
  rutas
}

testthat::test_that("la presentacion separa sentidos sin cambiar la clave", {
  secuencia <- data.table::data.table(
    id_version_variante = c("A", "A", "B", "B"),
    indice_secuencia = c(1:2, 1:2),
    punto_canonico_id = c("P1", "P2", "P2", "P1"),
    corredor_id = "MVD_1", indice_punto_corredor = c(1:2, 2:1),
    x_canonica = 0, y_canonica = c(10, 0, 0, 10)
  )
  orientacion <- data.table::data.table(
    corredor_id = "MVD_1", eje_orientacion = "NS",
    sentido_adelante = "N_S", sentido_atras = "S_N",
    confianza_orientacion = 1, fuente_orientacion = "prueba",
    sentido_aplicable = TRUE
  )
  tabla <- construir_tabla_presentacion(
    derivar_sentidos_ocurrencias(secuencia, orientacion), 10
  )
  testthat::expect_identical(
    tabla$sentido_recorrido, c("N_S", "N_S", "S_N", "S_N")
  )
  testthat::expect_equal(
    tabla[sentido_recorrido == "N_S", x_mapa - x_canonica], c(-10, -10)
  )
  testthat::expect_equal(
    tabla[sentido_recorrido == "S_N", x_mapa - x_canonica], c(10, 10)
  )
  testthat::expect_equal(data.table::uniqueN(tabla$id_presentacion), 4L)
})

testthat::test_that("el registro controla excepciones y es inmutable", {
  raiz <- tempfile("presentacion_")
  dir.create(raiz)
  rutas <- crear_fuentes_presentacion_prueba(raiz)
  argumentos <- list(
    version_presentacion = "prueba_v1",
    archivo_secuencias = rutas$secuencias,
    archivo_catalogo_puntos = rutas$puntos,
    archivo_referencia_orientacion_urbana = rutas$referencia,
    directorio_registro = rutas$registro,
    version_variantes = "variantes_prueba",
    snapshot_referencias = "snapshot_prueba",
    red_canonica = "red_prueba",
    vias_sin_sentido = 2354,
    desplazamiento_m = 10,
    esperados = list(
      posiciones = 5, posiciones_urbanas = 4,
      posiciones_urbanas_con_nombre = 4,
      corredores_exteriores_sin_nombre = 1,
      posiciones_exteriores_sin_nombre = 1,
      posiciones_sin_sentido = 2, posiciones_indeterminadas = 1,
      identidades_presentacion = 5
    )
  )
  resultado <- do.call(construir_registro_presentacion, argumentos)
  tabla <- data.table::as.data.table(arrow::read_parquet(file.path(
    resultado$destino, "secuencias_presentacion.parquet"
  )))
  testthat::expect_true(all(resultado$controles$aprobado))
  testthat::expect_true(all(
    tabla[corredor_id == "MVD_2354", sentido_recorrido] == "SIN_SENTIDO"
  ))
  testthat::expect_true(all(tabla[corredor_id == "MVD_2354",
    x_mapa == x_canonica & y_mapa == y_canonica]))
  testthat::expect_identical(
    tabla[corredor_id == "EXT_1", sentido_recorrido], "INDETERMINADO"
  )
  testthat::expect_identical(
    tabla[corredor_id == "EXT_1", clave_via], "CORRIDOR::EXT_1"
  )
  testthat::expect_error(
    do.call(construir_registro_presentacion, argumentos), "ya existe"
  )
})

testthat::test_that("la publicacion estatica no modifica dias publicados", {
  raiz <- tempfile("publicacion_presentacion_")
  dir.create(raiz)
  rutas <- crear_fuentes_presentacion_prueba(raiz)
  registro <- construir_registro_presentacion(
    version_presentacion = "prueba_v1",
    archivo_secuencias = rutas$secuencias,
    archivo_catalogo_puntos = rutas$puntos,
    archivo_referencia_orientacion_urbana = rutas$referencia,
    directorio_registro = rutas$registro,
    version_variantes = "variantes_prueba",
    snapshot_referencias = "snapshot_prueba",
    red_canonica = "red_prueba",
    vias_sin_sentido = 2354,
    esperados = list(posiciones = 5)
  )
  publicado <- file.path(raiz, "publicado")
  dia <- file.path(publicado, "dias", "fecha=2026-05-20")
  dir.create(dia, recursive = TRUE)
  testigo <- file.path(dia, "testigo.txt")
  writeLines("inmutable", testigo)
  huella_antes <- unname(tools::md5sum(testigo))

  resultado <- publicar_registro_presentacion(registro$destino, publicado)
  repetido <- publicar_registro_presentacion(registro$destino, publicado)

  testthat::expect_false(resultado$reutilizado)
  testthat::expect_true(repetido$reutilizado)
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "secuencias_presentacion.parquet"
  )))
  testthat::expect_identical(unname(tools::md5sum(testigo)), huella_antes)
})
