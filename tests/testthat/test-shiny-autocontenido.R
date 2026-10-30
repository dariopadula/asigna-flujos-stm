raiz_shiny <- file.path("..", "..", "Shiny")
source(file.path(raiz_shiny, "R", "consultas_temporales.R"))
source(file.path(raiz_shiny, "R", "adaptador_datos_publicados.R"))
source(file.path(raiz_shiny, "R", "datos_app.R"))
source(file.path(raiz_shiny, "R", "presentacion_mapas.R"))
source(file.path(raiz_shiny, "R", "consultas_flujo.R"))

testthat::test_that("la app carga solamente codigo interno", {
  codigo <- readLines(file.path(raiz_shiny, "app.R"), warn = FALSE)
  fuentes <- grep('^  ".*[.]R",?$', codigo, value = TRUE)
  fuentes <- sub('^  "', "", fuentes)
  fuentes <- sub('",?$', "", fuentes)
  testthat::expect_length(fuentes, 14L)
  testthat::expect_true(all(file.exists(file.path(raiz_shiny, fuentes))))
  testthat::expect_false(any(grepl("Shiny/beta|proyecto_flujo/Shiny", codigo)))
})

testthat::test_that("los mapas usan IM por defecto y conservan OSM", {
  modulos <- file.path(
    raiz_shiny, "modules", c("flow.R", "endpoint.R", "od.R")
  )
  codigo <- vapply(
    modulos,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1L)
  )
  mapa <- paste(readLines(
    file.path(raiz_shiny, "R", "presentacion_mapas.R"), warn = FALSE
  ), collapse = "\n")
  testthat::expect_true(all(grepl("add_beta_base_tiles()", codigo,
    fixed = TRUE)))
  testthat::expect_true(all(
    grepl("baseGroups = beta_base_groups()", codigo, fixed = TRUE) |
      grepl("baseGroups=beta_base_groups()", codigo, fixed = TRUE)
  ))
  testthat::expect_match(mapa, "montevideo.gub.uy", fixed = TRUE)
  testthat::expect_match(mapa, 'c("Mapa IM", "OSM")', fixed = TRUE)
})

testthat::test_that("la simbologia conserva escalas y saturacion validadas", {
  modulos <- file.path(
    raiz_shiny, "modules", c("flow.R", "endpoint.R", "od.R")
  )
  codigo <- vapply(
    modulos,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1L)
  )
  testthat::expect_true(all(grepl("addCircleMarkers", codigo, fixed = TRUE)))
  testthat::expect_true(all(grepl("beta_flow_radius", codigo, fixed = TRUE)))

  radios_flujo <- beta_flow_radius(c(0, 1, 4, 9, NA))
  testthat::expect_equal(min(radios_flujo), 1.5)
  testthat::expect_equal(max(radios_flujo), 4.5)
  testthat::expect_true(all(is.finite(radios_flujo)))

  visual <- beta_stop_visual(c(1, 4, 9, 16, 1000))
  testthat::expect_true(all(visual$radius >= 4 & visual$radius <= 11))
  testthat::expect_true(all(visual$opacity >= .55 & visual$opacity <= .9))
  testthat::expect_equal(max(visual$scaled), 1)

  paleta <- beta_stop_palette(120)
  colores <- NULL
  testthat::expect_warning(
    colores <- paleta(c(0, 115, 120, 724, NA)), NA
  )
  testthat::expect_identical(colores[3], colores[4])
  testthat::expect_identical(toupper(colores[4]), "#053061")
  testthat::expect_identical(colores[5], "transparent")

  titulo <- beta_stop_legend_title("buses", 12345)
  testthat::expect_match(titulo, "Paradas &middot; Buses", fixed = TRUE)
  testthat::expect_match(
    titulo, "M&aacute;x. color: 12.345+ &middot; p95", fixed = TRUE
  )

  ui <- paste(readLines(
    file.path(raiz_shiny, "components", "ui.R"), warn = FALSE
  ), collapse = "\n")
  testthat::expect_match(ui, "max-width:145px", fixed = TRUE)
  testthat::expect_match(ui, ".legend-detail", fixed = TRUE)
})

testthat::test_that("consulta conserva ventana semiabierta y filtro de linea", {
  inicio <- as.numeric(as.POSIXct(
    "2022-07-08", tz = "America/Montevideo"
  ))
  datos <- list(
    bus = data.table::data.table(
      recorrido = c("A", "B", "C"), ID_punto = "p",
      COD_LINEA = c("1", "2", "1"),
      tipo_servicio = c("urbano", "inter", "urbano"),
      tiempo = inicio + c(7, 8, 10) * 3600,
      ocupacion_personas = c(2, 0, 5), buses = 1,
      metodo_tiempo = "interpolado"
    ),
    points = data.table::data.table(
      ID_punto = "p", lon = -56, lat = -34,
      NOM_CALLE = "Calle", dirAv = "N_S"
    )
  )
  resultado <- query_beta_flow(datos, "2022-07-08", c(7, 10))
  testthat::expect_equal(resultado$points$personas, 2)
  testthat::expect_equal(resultado$points$buses, 2)
  testthat::expect_equal(
    query_beta_flow(datos, "2022-07-08", c(7, 10), "2", "personas")$puntos,
    0L
  )
  testthat::expect_equal(
    query_beta_flow(datos, "2022-07-08", c(7, 10), "2", "buses")$points$valor,
    1
  )
  testthat::expect_equal(
    query_beta_flow(
      datos, "2022-07-08", c(7, 10),
      metric = "buses", service_type = "urbano"
    )$points$valor,
    1
  )
  testthat::expect_equal(
    query_beta_flow(
      datos, "2022-07-08", c(7, 10),
      metric = "buses", service_type = "inter"
    )$points$valor,
    1
  )
  testthat::expect_equal(
    query_beta_flow(
      datos, "2022-07-08", c(7, 10),
      metric = "personas", service_type = "inter"
    )$puntos,
    0L
  )
})

testthat::test_that("el modulo de flujo expone el selector de servicio", {
  codigo <- paste(readLines(
    file.path(raiz_shiny, "modules", "flow.R"), warn = FALSE
  ), collapse = "\n")
  testthat::expect_match(codigo, 'ns("service_type")', fixed = TRUE)
  testthat::expect_match(codigo, '"Interdepartamental"="inter"', fixed = TRUE)
})

