source(file.path(
  "..", "..", "pipeline_referencias", "R", "fuentes",
  "preparar_fuentes_constructor.R"
))
source(file.path(
  "..", "..", "pipeline_referencias", "R", "canonica",
  "construir_base_urbana.R"
))
source(file.path(
  "..", "..", "pipeline_referencias", "R", "canonica",
  "preparar_red_osm.R"
))
source(file.path(
  "..", "..", "pipeline_referencias", "R", "canonica",
  "construir_red_hibrida.R"
))

test_that("la simplificacion urbana genera puntos identificables", {
  via <- sf::st_sf(
    NOM_CALLE = c("PRUEBA", "PRUEBA"),
    COD_NOMBRE = c(10L, 10L),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 5, 100, 5), ncol = 2, byrow = TRUE)),
      crs = 32721
    )
  )
  resultado <- simplificar_via_urbana(
    via, distancia_m = 25, celda_max_m = 50,
    partes_iniciales = 5, crs_metrico = 32721
  )
  expect_s3_class(resultado$linea, "sf")
  expect_s3_class(resultado$puntos, "sf")
  expect_gt(nrow(resultado$puntos), 1L)
  expect_equal(resultado$puntos$ID_calle, seq_len(nrow(resultado$puntos)))
  expect_true(all(c("dirIni", "dirFin") %in% names(resultado$puntos)))
})

test_that("la preparacion OSM separa interior y exterior", {
  raiz <- tempfile("osm_constructor_")
  dir.create(raiz)
  archivo_osm <- file.path(raiz, "osm.gpkg")
  vias <- sf::st_sf(
    osm_id = 1,
    name = "RUTA 1",
    highway = "primary",
    z_order = 1L,
    other_tags = '"ref"=>"1"',
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
      crs = 32721
    )
  )
  sf::st_write(vias, archivo_osm, layer = "lines", quiet = TRUE)
  muestras <- sf::st_sf(
    codigo_variante = c("1", "2"),
    nombre_linea = c("A", "B"),
    sentido = c("A", "A"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(150, 2)), crs = 32721)
  )
  limite <- sf::st_sf(
    ambito = "Montevideo",
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(-10, -10, 10, -10, 10, 10, -10, 10, -10, -10),
      ncol = 2, byrow = TRUE
    ))), crs = 32721)
  )
  resultado <- preparar_red_osm(muestras, limite, archivo_osm, 50, 1000)
  expect_equal(resultado$muestras$zona_red, c("montevideo", "exterior"))
  expect_equal(resultado$muestras$osm_id[[2]], "1")
  expect_equal(nrow(resultado$vias), 1L)
})

test_that("la union hibrida conserva IDs unicos y relaciones", {
  muestras <- sf::st_sf(
    osm_id = "1", zona_red = "exterior",
    geometry = sf::st_sfc(sf::st_point(c(150, 0)), crs = 32721)
  )
  vias <- sf::st_sf(
    osm_id = 1, name = "RUTA 1", other_tags = '"ref"=>"1"',
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
      crs = 32721
    )
  )
  limite <- sf::st_sf(
    ambito = "Montevideo",
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(-10, -10, 10, -10, 10, 10, -10, 10, -10, -10),
      ncol = 2, byrow = TRUE
    ))), crs = 32721)
  )
  calles_urbanas <- sf::st_sf(
    COD_NOMBRE = 10L, NOM_CALLE = "URBANA",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 10, 0), ncol = 2, byrow = TRUE)),
      crs = 32721
    )
  )
  puntos_urbanos <- sf::st_sf(
    COD_NOMBRE = c(10L, 10L), NOM_CALLE = c("URBANA", "URBANA"),
    ID_calle = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(10, 0)), crs = 32721)
  )
  resultado <- construir_red_hibrida(
    muestras, vias, limite, calles_urbanas, puntos_urbanos,
    "v1", "osm.gpkg", 202605L, 45, 25, 40, 32721
  )
  expect_equal(anyDuplicated(resultado$corredores$corridor_id), 0L)
  expect_equal(anyDuplicated(resultado$puntos$canonical_point_id), 0L)
  expect_true(all(resultado$puntos$corridor_id %in% resultado$corredores$corridor_id))
  expect_setequal(unique(resultado$corredores$network_zone), c("montevideo", "exterior"))
})

test_that("las fuentes copiadas conservan su huella", {
  raiz <- tempfile("fuentes_constructor_")
  dir.create(raiz)
  origen <- file.path(raiz, "origen.csv")
  writeLines("a,b\n1,2", origen)
  destino <- file.path(raiz, "destino", "copia.csv")
  resultado <- copiar_fuente_constructor(origen, destino)
  expect_equal(resultado$md5_origen, resultado$md5_destino)
  expect_true(file.exists(destino))
})
