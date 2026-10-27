source(file.path(
  "..", "..", "pipeline_referencias", "R", "variantes",
  "construir_variantes_canonicas.R"
))
source(file.path(
  "..", "..", "pipeline_referencias", "R", "variantes",
  "gestionar_variantes_canonicas.R"
))

testthat::test_that("las corridas cortas entre el mismo corredor se suavizan", {
  entrada <- c("A", "A", "B", "A", "A", "C", "C")
  testthat::expect_identical(
    suavizar_corridas_cortas(entrada, 1L),
    c("A", "A", "A", "A", "A", "C", "C")
  )
})

crear_red_sintetica_variantes <- function() {
  corredores <- sf::st_sf(
    corridor_id = c("MVD_1", "EXT_1"),
    network_zone = c("montevideo", "exterior"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 20, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(100, 0, 120, 0), ncol = 2, byrow = TRUE)),
      crs = 32721
    )
  )
  puntos <- sf::st_sf(
    canonical_point_id = c("M1", "M2", "M3", "E1", "E2", "E3"),
    corridor_id = rep(c("MVD_1", "EXT_1"), each = 3),
    corridor_point_index = rep(1:3, 2),
    osm_name = c(rep("URBANA", 3), rep("EXTERIOR", 3)),
    network_zone = rep(c("montevideo", "exterior"), each = 3),
    geometry = sf::st_sfc(lapply(
      list(c(0, 0), c(10, 0), c(20, 0), c(100, 0), c(110, 0), c(120, 0)),
      sf::st_point
    ), crs = 32721)
  )
  limite <- sf::st_sf(
    ambito = "Montevideo",
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(-10, -10, 30, -10, 30, 10, -10, 10, -10, -10),
      ncol = 2, byrow = TRUE
    ))), crs = 32721)
  )
  muestras <- sf::st_sf(
    indice_muestra = rep(1:3, 2),
    fraccion = rep(c(0, 0.5, 1), 2),
    codigo_variante = rep(c("V1", "V2"), each = 3),
    id_version_variante = rep(c("V1_1", "V2_1"), each = 3),
    codigo_linea = rep(c("L1", "L2"), each = 3),
    nombre_linea = rep(c("1", "2"), each = 3),
    sentido = "A",
    geometry = sf::st_sfc(lapply(
      list(c(0, 1), c(10, 1), c(20, 1), c(100, 1), c(110, 1), c(120, 1)),
      sf::st_point
    ), crs = 32721)
  )
  list(corredores = corredores, puntos = puntos, limite = limite, muestras = muestras)
}

testthat::test_that("las variantes se proyectan sobre puntos del catalogo por zona", {
  red <- crear_red_sintetica_variantes()
  resultado <- construir_secuencias_variantes(
    red$muestras, red$limite, red$corredores, red$puntos,
    distancia_aceptada_m = 2, distancia_revision_m = 5
  )
  tabla <- data.table::as.data.table(sf::st_drop_geometry(resultado$secuencias))
  testthat::expect_equal(nrow(tabla), 6L)
  testthat::expect_setequal(tabla$punto_canonico_id, c("M1", "M2", "M3", "E1", "E2", "E3"))
  testthat::expect_equal(tabla[, max(indice_secuencia), by = id_version_variante]$V1, c(3L, 3L))
  testthat::expect_true(all(resultado$calidad$proporcion_aceptada == 1))
})

testthat::test_that("la proyeccion por lotes conserva las mismas secuencias", {
  red <- crear_red_sintetica_variantes()
  completa <- construir_secuencias_variantes(
    red$muestras, red$limite, red$corredores, red$puntos,
    distancia_aceptada_m = 2, distancia_revision_m = 5,
    cada_variantes = 100L
  )
  por_variante <- construir_secuencias_variantes(
    red$muestras, red$limite, red$corredores, red$puntos,
    distancia_aceptada_m = 2, distancia_revision_m = 5,
    cada_variantes = 1L
  )
  columnas <- c("id_version_variante", "indice_secuencia", "punto_canonico_id")
  tabla_completa <- data.table::as.data.table(
    sf::st_drop_geometry(completa$secuencias)
  )[, ..columnas]
  tabla_lotes <- data.table::as.data.table(
    sf::st_drop_geometry(por_variante$secuencias)
  )[, ..columnas]
  testthat::expect_equal(tabla_lotes, tabla_completa)
})

testthat::test_that("las paradas quedan vinculadas a la secuencia de su version", {
  red <- crear_red_sintetica_variantes()
  proyeccion <- construir_secuencias_variantes(
    red$muestras, red$limite, red$corredores, red$puntos,
    distancia_aceptada_m = 2, distancia_revision_m = 5
  )
  universo <- data.table::data.table(
    codigo_variante = c("V1", "V2"),
    id_version_variante = c("V1_1", "V2_1")
  )
  paradas <- data.table::data.table(
    cod_variante = c("V1", "V1", "V2", "V2"),
    cod_ubic_parada = paste0("P", 1:4),
    ordinal = c(1, 2, 1, 2),
    x = c(0, 20, 100, 120),
    y = c(1, 1, 1, 1)
  )
  resultado <- asignar_paradas_variantes(
    paradas, universo, proyeccion$secuencias, red$puntos,
    crs_geografico = 32721, distancia_aceptada_m = 2, distancia_revision_m = 5
  )
  tabla <- data.table::as.data.table(sf::st_drop_geometry(resultado$paradas))
  testthat::expect_identical(tabla$punto_canonico_id, c("M1", "M3", "E1", "E3"))
  testthat::expect_true(all(tabla$estado_calidad == "aceptada"))
  controles <- validar_variantes_canonicas(
    universo, proyeccion$secuencias, resultado$paradas, red$puntos
  )
  testthat::expect_true(all(controles$aprobado))
})

testthat::test_that("la interpolacion acepta grupos con medianas enteras y decimales", {
  red <- crear_red_sintetica_variantes()
  proyeccion <- construir_secuencias_variantes(
    red$muestras, red$limite, red$corredores, red$puntos,
    distancia_aceptada_m = 2, distancia_revision_m = 5
  )
  universo <- data.table::data.table(
    codigo_variante = c("V1", "V2"),
    id_version_variante = c("V1_1", "V2_1")
  )
  paradas <- data.table::data.table(
    cod_variante = c(rep("V1", 4), rep("V2", 3)),
    cod_ubic_parada = paste0("P", 1:7),
    ordinal = c(1, 1, 2, 3, 1, 2, 3),
    x = c(0, 0, 500, 20, 100, 500, 120),
    y = 1
  )
  resultado <- asignar_paradas_variantes(
    paradas, universo, proyeccion$secuencias, red$puntos,
    crs_geografico = 32721, distancia_aceptada_m = 2, distancia_revision_m = 5
  )
  testthat::expect_equal(nrow(resultado$paradas), 7L)
  testthat::expect_false(anyNA(resultado$paradas$punto_canonico_id))
  testthat::expect_true(any(resultado$paradas$metodo_asignacion == "interpolacion_ordinal"))
})

testthat::test_that("los checkpoints se promueven completos y se reutilizan", {
  raiz <- tempfile("checkpoint_variantes_")
  destino <- file.path(raiz, "01_proyeccion")
  llamadas <- 0L
  escritor <- function(temporal) {
    llamadas <<- llamadas + 1L
    writeLines("contenido", file.path(temporal, "resultado.txt"))
  }
  primero <- escribir_checkpoint_variantes(
    destino, "prueba", "resultado.txt", escritor,
    list(registros = 1L)
  )
  segundo <- escribir_checkpoint_variantes(
    destino, "prueba", "resultado.txt", escritor,
    list(registros = 1L)
  )
  testthat::expect_false(primero$reutilizado)
  testthat::expect_true(segundo$reutilizado)
  testthat::expect_equal(llamadas, 1L)
  testthat::expect_true(checkpoint_variantes_completo(
    destino, "prueba", "resultado.txt"
  ))
  testthat::expect_true(file.exists(file.path(destino, "manifiesto.yml")))
})

testthat::test_that("una salida incompleta no se reutiliza ni se sobrescribe", {
  destino <- tempfile("checkpoint_incompleto_")
  dir.create(destino, recursive = TRUE)
  writeLines("parcial", file.path(destino, "resultado.txt"))
  testthat::expect_false(checkpoint_variantes_completo(
    destino, "prueba", "resultado.txt"
  ))
  testthat::expect_error(
    escribir_checkpoint_variantes(
      destino, "prueba", "resultado.txt",
      function(temporal) writeLines("nuevo", file.path(temporal, "resultado.txt"))
    ),
    "no sera sobrescrita"
  )
  testthat::expect_identical(
    readLines(file.path(destino, "resultado.txt")), "parcial"
  )
})

testthat::test_that("el manifiesto debe corresponder a la etapa solicitada", {
  destino <- tempfile("checkpoint_etapa_")
  escribir_checkpoint_variantes(
    destino, "etapa_a", "resultado.txt",
    function(temporal) writeLines("ok", file.path(temporal, "resultado.txt"))
  )
  testthat::expect_true(checkpoint_variantes_completo(
    destino, "etapa_a", "resultado.txt"
  ))
  testthat::expect_false(checkpoint_variantes_completo(
    destino, "etapa_b", "resultado.txt"
  ))
})

testthat::test_that("la comparacion con el oraculo usa la clave canonica completa", {
  archivo <- tempfile(fileext = ".csv")
  data.table::fwrite(data.table::data.table(
    variant_version_id = c("V1_1", "V1_1"),
    sequence_index = c(1L, 2L),
    canonical_point_id = c("P1", "P2")
  ), archivo)
  actual <- data.table::data.table(
    id_version_variante = c("V1_1", "V1_1"),
    indice_secuencia = c(1L, 2L),
    punto_canonico_id = c("P1", "P2")
  )
  iguales <- comparar_secuencias_oraculo(actual, archivo)
  testthat::expect_true(iguales$igualdad_exacta)
  actual[2L, punto_canonico_id := "P3"]
  diferentes <- comparar_secuencias_oraculo(actual, archivo)
  testthat::expect_false(diferentes$igualdad_exacta)
  testthat::expect_equal(nrow(diferentes$faltantes), 1L)
  testthat::expect_equal(nrow(diferentes$adicionales), 1L)
})
