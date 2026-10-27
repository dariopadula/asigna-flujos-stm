source(file.path(
  "..", "..", "pipeline_referencias", "R", "regiones",
  "construir_registro_regiones.R"
))
source(file.path(
  "..", "..", "pipeline_diario", "R", "publicacion", "publicar_dia.R"
))

crear_poligono_prueba <- function(xmin, xmax, nombre, columna) {
  geometria <- sf::st_polygon(list(matrix(c(
    xmin, 0, xmax, 0, xmax, 1, xmin, 1, xmin, 0
  ), ncol = 2, byrow = TRUE)))
  datos <- data.frame(valor = nombre)
  names(datos) <- columna
  sf::st_sf(datos, geometry = sf::st_sfc(geometria, crs = 4326))
}

crear_registro_regiones_prueba <- function(raiz) {
  fuentes <- file.path(raiz, "fuentes")
  dir.create(fuentes, recursive = TRUE, showWarnings = FALSE)
  rutas <- c(
    barrios = file.path(fuentes, "barrios.shp"),
    ccz = file.path(fuentes, "ccz.shp"),
    municipios = file.path(fuentes, "municipios.shp"),
    departamentos = file.path(fuentes, "departamentos.shp")
  )
  if (!file.exists(rutas[["barrios"]])) {
    sf::st_write(
      crear_poligono_prueba(0, 1, "CENTRO", "NOMBBARR"),
      rutas[["barrios"]], quiet = TRUE
    )
    sf::st_write(
      crear_poligono_prueba(0, 1, "CCZ1", "ZONA_LEGAL"),
      rutas[["ccz"]], quiet = TRUE
    )
    sf::st_write(
      crear_poligono_prueba(0, 1, "B", "MUNICIPIO"),
      rutas[["municipios"]], quiet = TRUE
    )
    departamentos <- rbind(
      crear_poligono_prueba(0, 1, "MONTEVIDEO", "NOMBRE"),
      crear_poligono_prueba(2, 3, "CANELONES", "NOMBRE"),
      crear_poligono_prueba(4, 5, "SAN JOSE", "NOMBRE")
    )
    sf::st_write(departamentos, rutas[["departamentos"]], quiet = TRUE)
  }
  archivo_paradas <- file.path(raiz, "paradas_proyectadas.parquet")
  if (!file.exists(archivo_paradas)) {
    arrow::write_parquet(data.table::data.table(
      cod_ubic_parada = rep(1:4, each = 2),
      desc_ubic_parada = rep(c("A", "B", "C", "D"), each = 2),
      x = rep(c(0.25, 2.25, 4.25, 6.25), each = 2),
      y = 0.25
    ), archivo_paradas)
  }
  construir_registro_regiones(
    version_regiones = "regiones_prueba_v1",
    archivo_paradas_canonicas = archivo_paradas,
    archivo_barrios = rutas[["barrios"]],
    archivo_ccz = rutas[["ccz"]],
    archivo_municipios = rutas[["municipios"]],
    archivo_departamentos = rutas[["departamentos"]],
    directorio_registro = file.path(raiz, "registro_regiones"),
    version_variantes = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1"
  )
}

crear_entradas_publicacion <- function(raiz) {
  fecha <- "2026-05-20"
  region <- crear_registro_regiones_prueba(raiz)
  ocupacion <- file.path(raiz, "ocupacion")
  insumos <- file.path(raiz, "insumos")
  red <- file.path(raiz, "red")
  dir.create(file.path(ocupacion, "pasadas_ocupacion"), recursive = TRUE)
  dir.create(file.path(insumos, "actividad_paradas"), recursive = TRUE)
  dir.create(file.path(insumos, "viajes_consulta"), recursive = TRUE)
  dir.create(red)
  recorrido <- paste(fecha, "1", "100", sep = "|")
  bus <- data.table::data.table(
    recorrido = recorrido,
    tipo_servicio = "urbano",
    indice_secuencia = 1:2,
    punto_canonico_id = c("P1", "P2"),
    tiempo = c(1000, 1060),
    buses = 1L,
    ocupacion_personas = c(2, 0)
  )
  actividad <- data.table::data.table(
    fecha_operativa = as.Date(fecha),
    recorrido = recorrido,
    tipo_servicio = "urbano",
    codigo_parada = c("1", "2"),
    indice_secuencia = 1:2,
    buses = 1L,
    subidas = c(2, 0),
    bajadas = c(0, 2),
    ocupacion_personas = c(2, 0)
  )
  viajes <- data.table::data.table(
    fecha_operativa = as.Date(fecha),
    trip_id = 1L,
    recorrido = recorrido,
    tipo_servicio = "urbano",
    codigo_parada_origen = "1",
    codigo_parada_destino = "2",
    cantidad_pasajeros = 2
  )
  arrow::write_parquet(
    bus, file.path(ocupacion, "pasadas_ocupacion", "part-00001.parquet")
  )
  arrow::write_parquet(
    actividad,
    file.path(insumos, "actividad_paradas", "part-00001.parquet")
  )
  arrow::write_parquet(
    viajes, file.path(insumos, "viajes_consulta", "part-00001.parquet")
  )
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1",
    cantidad_lotes = 1L,
    recorridos = 1L,
    filas_pasadas = 2L,
    controles_fallidos = 0L
  ), file.path(ocupacion, "manifiesto.yml"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1",
    cantidad_lotes = 1L,
    recorridos = 1L,
    filas_actividad = 2L,
    registros_viajes = 1L,
    personas = 2,
    subidas = 2,
    bajadas = 2,
    pasadas_bus = 2,
    controles_fallidos = 0L
  ), file.path(insumos, "manifiesto.yml"))
  saveRDS(data.frame(id = "P1"), file.path(red, "catalogo_puntos.rds"))
  saveRDS(data.frame(id = "C1"), file.path(red, "catalogo_corredores.rds"))
  yaml::write_yaml(list(
    version_red = "canonica_prueba_v1"
  ), file.path(red, "manifiesto.yml"))
  list(
    fecha = fecha,
    ocupacion = ocupacion,
    insumos = insumos,
    regiones = region$destino,
    red = red,
    publicado = file.path(raiz, "published")
  )
}

testthat::test_that("el registro regional se construye una sola vez", {
  raiz <- tempfile("regiones_")
  dir.create(raiz)
  resultado <- crear_registro_regiones_prueba(raiz)
  testthat::expect_equal(nrow(resultado$regiones), 4L)
  testthat::expect_equal(
    resultado$regiones[codigo_parada == "1", departamento],
    "MONTEVIDEO"
  )
  testthat::expect_equal(
    resultado$regiones[codigo_parada == "2", departamento],
    "CANELONES"
  )
  testthat::expect_true(all(resultado$controles$aprobado))
  testthat::expect_error(
    crear_registro_regiones_prueba(raiz),
    "ya existe"
  )
})

testthat::test_that(
  "la publicacion regionaliza sin recalcular ocupacion",
  {
    raiz <- tempfile("publicacion_")
    dir.create(raiz)
    entradas <- crear_entradas_publicacion(raiz)
    resultado <- publicar_dia(
      fecha = entradas$fecha,
      directorio_ocupacion = entradas$ocupacion,
      directorio_insumos = entradas$insumos,
      directorio_regiones = entradas$regiones,
      directorio_red_canonica = entradas$red,
      directorio_publicado = entradas$publicado
    )
    actividad <- data.table::as.data.table(arrow::read_parquet(file.path(
      resultado$destino, "actividad_paradas", "part-00001.parquet"
    )))
    viajes <- data.table::as.data.table(arrow::read_parquet(file.path(
      resultado$destino, "viajes_consulta", "part-00001.parquet"
    )))
    testthat::expect_equal(actividad[codigo_parada == "1", barrio], "CENTRO")
    testthat::expect_equal(
      viajes$departamento_origen, "MONTEVIDEO"
    )
    testthat::expect_equal(
      viajes$departamento_destino, "CANELONES"
    )
    testthat::expect_equal(resultado$resumen$personas, 2)
    testthat::expect_equal(sum(resultado$controles$aprobado), 23L)
    testthat::expect_true(file.exists(resultado$archivo_catalogo))
    testthat::expect_identical(
      unname(tools::md5sum(file.path(
        entradas$ocupacion, "pasadas_ocupacion", "part-00001.parquet"
      ))),
      unname(tools::md5sum(file.path(
        resultado$destino, "pasadas_ocupacion", "part-00001.parquet"
      )))
    )
    testthat::expect_error(
      publicar_dia(
        fecha = entradas$fecha,
        directorio_ocupacion = entradas$ocupacion,
        directorio_insumos = entradas$insumos,
        directorio_regiones = entradas$regiones,
        directorio_red_canonica = entradas$red,
        directorio_publicado = entradas$publicado
      ),
      "ya existe"
    )
  }
)
