source(file.path(
  "..", "..", "Shiny", "R", "adaptador_datos_publicados.R"
))

crear_publicacion_shiny_prueba <- function(raiz) {
  publicado <- file.path(raiz, "published")
  catalogo <- file.path(publicado, "catalogo")
  dia_rel <- file.path("dias", "fecha=2026-05-20")
  dia <- file.path(publicado, dia_rel)
  regiones_rel <- file.path(
    "referencias", "red=red_prueba", "regiones=regiones_prueba"
  )
  presentacion_rel <- file.path(
    "referencias", "red=red_prueba", "presentacion=presentacion_prueba"
  )
  regiones <- file.path(publicado, regiones_rel)
  presentacion <- file.path(publicado, presentacion_rel)
  dir.create(catalogo, recursive = TRUE)
  for (subdir in c("pasadas_ocupacion", "actividad_paradas",
      "viajes_consulta")) {
    dir.create(file.path(dia, subdir), recursive = TRUE)
  }
  dir.create(regiones, recursive = TRUE)
  dir.create(presentacion, recursive = TRUE)

  yaml::write_yaml(list(
    fecha_operativa = "2026-05-20", estado = "listo",
    directorio = dia_rel, directorio_referencias = regiones_rel,
    archivo_manifiesto = "manifiesto.yml",
    directorio_ocupacion = "pasadas_ocupacion",
    directorio_actividad = "actividad_paradas",
    directorio_viajes = "viajes_consulta",
    recorridos = 1L, registros_viajes = 1L, personas = 2L
  ), file.path(catalogo, "fecha=2026-05-20.yml"))
  yaml::write_yaml(list(
    fecha_operativa = "2026-05-21", estado = "procesando",
    directorio = "dias/fecha=2026-05-21",
    directorio_referencias = regiones_rel,
    archivo_manifiesto = "manifiesto.yml",
    directorio_ocupacion = "pasadas_ocupacion",
    directorio_actividad = "actividad_paradas",
    directorio_viajes = "viajes_consulta",
    recorridos = 1L, registros_viajes = 1L, personas = 1L
  ), file.path(catalogo, "fecha=2026-05-21.yml"))
  yaml::write_yaml(list(
    fecha_operativa = "2026-05-20", estado = "listo",
    version_registro_variantes = "variantes_prueba",
    snapshot_referencias = "snapshot_prueba", red_canonica = "red_prueba",
    controles_fallidos = 0L, filas_bus = 2L, filas_actividad = 2L
  ), file.path(dia, "manifiesto.yml"))

  presentacion_tabla <- data.table::data.table(
    id_version_variante = "V1_1", corredor_id = "MVD_1",
    sentido_recorrido = "N_S", metodo_sentido = "orden_canonico_tramo",
    delta_x_sentido = 0, delta_y_sentido = -10,
    codigo_variante = "V1", codigo_linea = "L1", nombre_linea = "1",
    sentido_variante = "A", indice_secuencia = 1:2,
    punto_canonico_id = c("P1", "P2"), indice_punto_corredor = 1:2,
    nombre_via_fuente = "Calle", zona_red = "montevideo",
    x_canonica = 500000, y_canonica = c(6140000, 6139990),
    clave_via = "NAME::CALLE", nombre_via = "Calle",
    nombre_via_disponible = TRUE, eje_orientacion = "NS",
    sentido_adelante = "N_S", sentido_atras = "S_N",
    confianza_orientacion = 1, fuente_orientacion = "prueba",
    sentido_aplicable = TRUE,
    id_presentacion = paste0(c("P1", "P2"), "::N_S"),
    x_mapa = 499990, y_mapa = c(6140000, 6139990)
  )
  arrow::write_parquet(
    presentacion_tabla,
    file.path(presentacion, "secuencias_presentacion.parquet")
  )
  arrow::write_parquet(data.table::data.table(
    clave_via = "NAME::CALLE", etiqueta_via = "Calle · Montevideo",
    corredores = 1L, con_nombre = TRUE
  ), file.path(presentacion, "catalogo_vias.parquet"))
  yaml::write_yaml(list(
    tipo = "referencia_presentacion_publicada", estado = "listo",
    version_presentacion = "presentacion_prueba",
    version_registro_variantes = "variantes_prueba",
    snapshot_referencias = "snapshot_prueba", red_canonica = "red_prueba"
  ), file.path(presentacion, "manifiesto_publicacion.yml"))

  arrow::write_parquet(data.table::data.table(
    codigo_parada = c("10", "20"), nombre_parada = c("A", "B"),
    longitud = c(-56.1, -56.2), latitud = c(-34.8, -34.9),
    barrio = c("CENTRO", "CORDON"), ccz = c("1", "2"),
    municipio = c("B", "B"), departamento = "MONTEVIDEO"
  ), file.path(regiones, "paradas_regiones.parquet"))
  punto_sf <- function(columna, valor) {
    x <- sf::st_sf(valor = valor, geometry = sf::st_sfc(
      sf::st_point(c(-56.1, -34.8)), crs = 4326
    ))
    names(x)[names(x) == "valor"] <- columna
    x
  }
  saveRDS(list(
    barrios = punto_sf("barrio", "CENTRO"),
    ccz = punto_sf("ccz", "1"),
    municipios = punto_sf("municipio", "B"),
    departamentos = punto_sf("departamento", "MONTEVIDEO")
  ), file.path(regiones, "formas_regiones.rds"))

  bus <- data.table::data.table(
    recorrido = "2026-05-20|1", id_version_variante = "V1_1",
    tipo_servicio = "urbano", indice_secuencia = 1:2,
    punto_canonico_id = c("P1", "P2"), distancia_m = c(0, 10),
    tiempo = as.POSIXct("2026-05-20 08:00:00", tz = "UTC") + 0:1 * 10,
    metodo_tiempo = "interpolado", buses = 1L,
    subidas = c(2, 0), bajadas = c(0, 2),
    ocupacion_personas = c(2, 0)
  )
  arrow::write_parquet(bus[1L], file.path(
    dia, "pasadas_ocupacion", "part-00001.parquet"
  ))
  arrow::write_parquet(bus[2L], file.path(
    dia, "pasadas_ocupacion", "part-00002.parquet"
  ))
  actividad <- data.table::data.table(
    codigo_parada = c("10", "20"), fecha_operativa = as.Date("2026-05-20"),
    recorrido = "2026-05-20|1", id_version_variante = "V1_1",
    codigo_variante = "V1", codigo_linea = "L1", codigo_empresa = 1L,
    codigo_subsistema = 1L, tipo_servicio = "urbano",
    indice_secuencia = 1:2, punto_canonico_id = c("P1", "P2"),
    distancia_m = c(0, 10), tiempo = bus$tiempo,
    metodo_tiempo = "interpolado", buses = 1L,
    subidas = c(2, 0), bajadas = c(0, 2), ocupacion_personas = c(2, 0),
    barrio = c("CENTRO", "CORDON"), ccz = c("1", "2"),
    municipio = "B", departamento = "MONTEVIDEO"
  )
  arrow::write_parquet(actividad[1L], file.path(
    dia, "actividad_paradas", "part-00001.parquet"
  ))
  arrow::write_parquet(actividad[2L], file.path(
    dia, "actividad_paradas", "part-00002.parquet"
  ))
  viajes <- data.table::data.table(
    codigo_parada_destino = "20", codigo_parada_origen = "10",
    fecha_operativa = as.Date("2026-05-20"), trip_id = 1L,
    recorrido = "2026-05-20|1", id_version_variante = "V1_1",
    codigo_variante = "V1", codigo_linea = "L1", codigo_empresa = 1L,
    codigo_subsistema = 1L, tipo_servicio = "urbano",
    indice_secuencia_origen = 1L, indice_secuencia_destino = 2L,
    punto_canonico_origen = "P1", punto_canonico_destino = "P2",
    tiempo_observado_subida = bus$tiempo[1],
    tiempo_observado_bajada = bus$tiempo[2],
    tiempo_estimado_subida = bus$tiempo[1],
    tiempo_estimado_bajada = bus$tiempo[2], cantidad_pasajeros = 2,
    recuperada_paso10 = FALSE, requiere_revision = FALSE,
    accion_recuperacion = NA_character_, barrio_origen = "CENTRO",
    ccz_origen = "1", municipio_origen = "B",
    departamento_origen = "MONTEVIDEO", barrio_destino = "CORDON",
    ccz_destino = "2", municipio_destino = "B",
    departamento_destino = "MONTEVIDEO"
  )
  arrow::write_parquet(viajes, file.path(
    dia, "viajes_consulta", "part-00001.parquet"
  ))
  publicado
}

testthat::test_that("el catalogo muestra solamente dias listos", {
  raiz <- tempfile("shiny_publicado_")
  dir.create(raiz)
  publicado <- crear_publicacion_shiny_prueba(raiz)
  catalogo <- leer_catalogo_publicado(publicado)

  testthat::expect_equal(nrow(catalogo), 1L)
  testthat::expect_identical(
    as.character(catalogo$fecha_operativa), "2026-05-20"
  )
})

testthat::test_that("la base carga solo referencias estaticas", {
  raiz <- tempfile("shiny_publicado_")
  dir.create(raiz)
  publicado <- crear_publicacion_shiny_prueba(raiz)
  datos <- cargar_base_shiny_publicada(publicado, "presentacion_prueba")

  testthat::expect_identical(datos$profile, "publicado_limpio")
  testthat::expect_equal(nrow(datos$bus), 0L)
  testthat::expect_equal(nrow(datos$trips), 0L)
  testthat::expect_equal(nrow(datos$occurrences), 2L)
  testthat::expect_equal(nrow(datos$stops), 2L)
  testthat::expect_identical(datos$lines$COD_LINEA, "L1")
  testthat::expect_setequal(
    datos$zone_choices$barrio, c("CENTRO", "CORDON")
  )
})

testthat::test_that("el dia se carga por partes y se adapta en el borde", {
  raiz <- tempfile("shiny_publicado_")
  dir.create(raiz)
  publicado <- crear_publicacion_shiny_prueba(raiz)
  base <- cargar_base_shiny_publicada(publicado, "presentacion_prueba")
  dia <- cargar_dia_shiny_publicado(base, "2026-05-20")
  repetido <- cargar_dia_shiny_publicado(base, "2026-05-20")

  testthat::expect_equal(nrow(dia$bus), 2L)
  testthat::expect_equal(nrow(dia$stop_passes), 2L)
  testthat::expect_equal(nrow(dia$trips), 1L)
  testthat::expect_true(all(c(
    "COD_VARIAN", "seqViajeN", "ID_punto", "ID_presentacion"
  ) %in% names(dia$bus)))
  testthat::expect_identical(dia$trips$seq_origen_expansion, 1L)
  testthat::expect_identical(
    dia$trips$COD_PARADA_DESTINO_EXPANSION, "20"
  )
  testthat::expect_identical(repetido$bus, dia$bus)
  testthat::expect_true(exists(
    "2026-05-20", envir = base$daily_cache, inherits = FALSE
  ))
})

testthat::test_that("el adaptador rechaza rutas fuera de published", {
  raiz <- tempfile("shiny_publicado_")
  dir.create(raiz)
  publicado <- crear_publicacion_shiny_prueba(raiz)

  testthat::expect_error(
    resolver_ruta_publicada(publicado, ".."),
    "fuera de data/published"
  )
})
