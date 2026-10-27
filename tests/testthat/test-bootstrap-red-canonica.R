source(file.path(
  "..", "..", "pipeline_referencias", "R", "canonica",
  "migrar_red_canonica_inicial.R"
))

test_that("el bootstrap conserva literalmente la red canonica", {
  raiz <- tempfile("bootstrap_canonico_")
  dir.create(raiz)
  corredores <- sf::st_sf(
    corridor_id = c("C1", "C2"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(2, 2, 3, 3), ncol = 2, byrow = TRUE)),
      crs = 32721
    )
  )
  puntos <- sf::st_sf(
    canonical_point_id = c("P1", "P2", "P3"),
    corridor_id = c("C1", "C1", "C2"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2)),
      crs = 32721
    )
  )
  archivo_corredores <- file.path(raiz, "corredores.rds")
  archivo_puntos <- file.path(raiz, "puntos.rds")
  saveRDS(corredores, archivo_corredores, compress = FALSE)
  saveRDS(puntos, archivo_puntos, compress = FALSE)

  resultado <- migrar_red_canonica_inicial(
    "v1", archivo_corredores, archivo_puntos,
    file.path(raiz, "registry"), 2L, 3L, 32721L
  )
  expect_true(all(resultado$manifiesto_archivos$copia_identica))
  expect_identical(
    unname(tools::md5sum(archivo_corredores)),
    unname(tools::md5sum(file.path(resultado$destino, "catalogo_corredores.rds")))
  )
  expect_identical(
    unname(tools::md5sum(archivo_puntos)),
    unname(tools::md5sum(file.path(resultado$destino, "catalogo_puntos.rds")))
  )
  expect_error(
    migrar_red_canonica_inicial(
      "v1", archivo_corredores, archivo_puntos,
      file.path(raiz, "registry"), 2L, 3L, 32721L
    ),
    "ya existe"
  )
})

test_that("el bootstrap rechaza puntos huerfanos", {
  raiz <- tempfile("bootstrap_huerfano_")
  dir.create(raiz)
  corredores <- sf::st_sf(
    corridor_id = "C1",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      crs = 32721
    )
  )
  puntos <- sf::st_sf(
    canonical_point_id = "P1",
    corridor_id = "C9",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 32721)
  )
  archivo_corredores <- file.path(raiz, "corredores.rds")
  archivo_puntos <- file.path(raiz, "puntos.rds")
  saveRDS(corredores, archivo_corredores)
  saveRDS(puntos, archivo_puntos)
  expect_error(
    migrar_red_canonica_inicial(
      "v1", archivo_corredores, archivo_puntos, file.path(raiz, "registry")
    ),
    "puntos sin corredor"
  )
})
