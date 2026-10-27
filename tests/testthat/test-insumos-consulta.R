source(file.path(
  "..", "..", "pipeline_diario", "R", "consultas",
  "preparar_insumos_consulta_dia.R"
))

crear_entradas_insumos <- function(raiz, alterar_snapshot = FALSE) {
  fecha <- "2026-05-20"
  version <- "variantes_prueba_v1"
  snapshot <- "referencias_prueba"
  red <- "canonica_prueba_v1"
  escribir_manifiesto <- function(directorio, fecha_diaria = TRUE) {
    contenido <- list(
      version_registro_variantes = version,
      snapshot_referencias = snapshot,
      red_canonica = red
    )
    if (fecha_diaria) contenido$fecha_operativa <- fecha
    yaml::write_yaml(contenido, file.path(directorio, "manifiesto.yml"))
  }

  od <- data.table::data.table(
    trip_id = 1:4,
    cod_empresa = "1",
    numero_evento_real_subida = c("100", "100", "200", "300"),
    fecha_evento_subida = c(
      "2026-05-20 10:00:00", "2026-05-20 10:20:00",
      "2026-05-20 11:00:00", "2026-05-20 12:00:00"
    ),
    fecha_evento_bajada = c(
      "2026-05-20 10:10:00", "2026-05-20 10:10:00",
      "2026-05-20 11:10:00", "2026-05-20 12:10:00"
    ),
    cantidad_pasajeros = c(2, 1, 3, 4),
    tipo_servicio = "urbano"
  )
  archivo_od <- file.path(raiz, "od_operativa.parquet")
  arrow::write_parquet(od, archivo_od)

  paso08 <- file.path(raiz, "paso08")
  dir.create(paso08)
  arrow::write_parquet(data.table::data.table(
    codigo_variante = "A",
    id_version_variante = "A_1",
    codigo_linea = "L1",
    codigo_subsistema = 1L
  ), file.path(paso08, "uso_variantes.parquet"))
  escribir_manifiesto(paso08)

  paso09 <- file.path(raiz, "paso09")
  dir.create(paso09)
  arrow::write_parquet(data.table::data.table(
    trip_id = 1:4,
    codigo_variante_subida = "A",
    codigo_parada_subida = c("10", "20", "10", "10"),
    codigo_parada_bajada = c("30", "40", "99", "40"),
    id_version_variante = "A_1",
    indice_secuencia_subida = c(1L, 2L, 1L, 1L),
    indice_secuencia_bajada = c(3L, 4L, NA_integer_, 4L),
    punto_canonico_subida = c("P1", "P2", "P1", "P1"),
    punto_canonico_bajada = c("P3", "P4", NA_character_, "P4"),
    estado_resolucion = c(
      "apta_secuencia", "apta_secuencia",
      "variante_subida_bajada_distinta", "apta_secuencia"
    )
  ), file.path(paso09, "resolucion_paradas.parquet"))
  escribir_manifiesto(paso09)

  paso10 <- file.path(raiz, "paso10")
  dir.create(paso10)
  arrow::write_parquet(data.table::data.table(
    trip_id = 3L,
    codigo_variante_subida = "A",
    id_version_variante_subida = "A_1",
    codigo_parada_subida_original = "10",
    indice_secuencia_subida = 1L,
    punto_canonico_subida = "P1",
    codigo_parada_bajada_asignada = "40",
    indice_secuencia_bajada_asignado = 4L,
    punto_canonico_bajada_asignado = "P4",
    recuperada = TRUE,
    requiere_revision = TRUE,
    accion_resolucion = "parada_cercana_en_variante_subida"
  ), file.path(paso10, "recuperaciones_paradas.parquet"))
  escribir_manifiesto(paso10)
  if (alterar_snapshot) {
    manifiesto <- yaml::read_yaml(file.path(paso10, "manifiesto.yml"))
    manifiesto$snapshot_referencias <- "otro_snapshot"
    yaml::write_yaml(manifiesto, file.path(paso10, "manifiesto.yml"))
  }

  paso11 <- file.path(raiz, "paso11")
  dir.create(paso11)
  recorridos <- paste(fecha, "1", c("100", "200", "300"), sep = "|")
  arrow::write_parquet(data.table::data.table(
    recorrido = recorridos,
    cod_empresa = "1",
    id_version_variante = "A_1",
    tipo_servicio = "urbano"
  ), file.path(paso11, "recorridos_operativos.parquet"))
  escribir_manifiesto(paso11)

  paradas <- file.path(raiz, "paradas")
  dir.create(paradas)
  arrow::write_parquet(data.table::data.table(
    id_version_variante = "A_1",
    cod_ubic_parada = c("10", "20", "21", "30", "40"),
    punto_canonico_id = c("P1", "P2", "P2", "P3", "P4"),
    indice_secuencia = c(1L, 2L, 2L, 3L, 4L)
  ), file.path(paradas, "paradas_proyectadas.parquet"))
  yaml::write_yaml(list(
    version = version,
    snapshot_referencias = snapshot,
    red_canonica = red
  ), file.path(paradas, "manifiesto.yml"))

  paso13 <- file.path(raiz, "paso13")
  dir.create(file.path(paso13, "pasadas_ocupacion"), recursive = TRUE)
  dir.create(file.path(paso13, "calidad_ocupacion"), recursive = TRUE)
  crear_pasadas <- function(recorrido, inicio, subidas, bajadas) {
    data.table::data.table(
      recorrido = recorrido,
      id_version_variante = "A_1",
      tipo_servicio = "urbano",
      indice_secuencia = 1:4,
      punto_canonico_id = paste0("P", 1:4),
      distancia_m = c(0, 100, 200, 300),
      tiempo = inicio + 0:3 * 20,
      metodo_tiempo = "ancla_resumida",
      buses = 1L,
      subidas = subidas,
      bajadas = bajadas,
      ocupacion_personas = cumsum(subidas - bajadas)
    )
  }
  arrow::write_parquet(
    crear_pasadas(recorridos[[1L]], 1000, c(2, 0, 0, 0), c(0, 0, 2, 0)),
    file.path(paso13, "pasadas_ocupacion", "part-00001.parquet")
  )
  arrow::write_parquet(
    crear_pasadas(recorridos[[2L]], 2000, c(3, 0, 0, 0), c(0, 0, 0, 3)),
    file.path(paso13, "pasadas_ocupacion", "part-00002.parquet")
  )
  calidad <- data.table::data.table(
    recorrido = recorridos[1:2],
    id_version_variante = "A_1",
    tipo_servicio = "urbano",
    registros_ocupacion = c(1L, 1L),
    personas_ocupacion = c(2, 3),
    subidas = c(2, 3),
    bajadas = c(2, 3)
  )
  arrow::write_parquet(
    calidad[1],
    file.path(paso13, "calidad_ocupacion", "part-00001.parquet")
  )
  arrow::write_parquet(
    calidad[2],
    file.path(paso13, "calidad_ocupacion", "part-00002.parquet")
  )
  arrow::write_parquet(data.table::data.table(
    trip_id = 2L,
    cantidad_pasajeros = 1
  ), file.path(paso13, "viajes_cuarentena_tiempo_invertido.parquet"))
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    version_registro_variantes = version,
    snapshot_referencias = snapshot,
    red_canonica = red,
    controles_fallidos = 0L
  ), file.path(paso13, "manifiesto.yml"))

  list(
    fecha = fecha,
    od = archivo_od,
    paso08 = paso08,
    paso09 = paso09,
    paso10 = paso10,
    paso11 = paso11,
    paso13 = paso13,
    paradas = paradas
  )
}

ejecutar_insumos_prueba <- function(entradas, raiz) {
  preparar_insumos_consulta_dia(
    fecha = entradas$fecha,
    archivo_od_operativa = entradas$od,
    directorio_resolucion_paso09 = entradas$paso09,
    directorio_recuperacion_paso10 = entradas$paso10,
    directorio_recorridos_paso11 = entradas$paso11,
    directorio_uso_variantes_paso08 = entradas$paso08,
    directorio_ocupacion_paso13 = entradas$paso13,
    directorio_paradas_canonicas = entradas$paradas,
    directorio_salida = file.path(raiz, "intermediate")
  )
}

testthat::test_that(
  "se preparan actividad fisica y viajes compactos sin recalcular ocupacion",
  {
    raiz <- tempfile("insumos_consulta_")
    dir.create(raiz)
    entradas <- crear_entradas_insumos(raiz)
    resultado <- ejecutar_insumos_prueba(entradas, raiz)

    actividad <- data.table::rbindlist(lapply(list.files(
      file.path(resultado$destino, "actividad_paradas"),
      pattern = "[.]parquet$", full.names = TRUE
    ), function(x) data.table::as.data.table(arrow::read_parquet(x))))
    viajes <- data.table::rbindlist(lapply(list.files(
      file.path(resultado$destino, "viajes_consulta"),
      pattern = "[.]parquet$", full.names = TRUE
    ), function(x) data.table::as.data.table(arrow::read_parquet(x))))

    testthat::expect_equal(nrow(actividad), 10L)
    testthat::expect_equal(nrow(viajes), 2L)
    testthat::expect_setequal(viajes$trip_id, c(1L, 3L))
    testthat::expect_equal(sum(viajes$cantidad_pasajeros), 5)
    testthat::expect_equal(sum(actividad$subidas), 5)
    testthat::expect_equal(sum(actividad$bajadas), 5)
    testthat::expect_equal(sum(actividad$buses), 10)
    testthat::expect_equal(
      viajes[trip_id == 1L, tiempo_estimado_subida], 1000
    )
    testthat::expect_equal(
      viajes[trip_id == 1L, tiempo_estimado_bajada], 1040
    )
    testthat::expect_true(viajes[trip_id == 3L, recuperada_paso10])
    testthat::expect_equal(
      actividad[
        recorrido == paste(entradas$fecha, "1", "100", sep = "|") &
          codigo_parada == "10",
        subidas
      ],
      2
    )
    testthat::expect_equal(sum(resultado$controles$aprobado), 19L)
    testthat::expect_true(all(resultado$controles$aprobado))
    testthat::expect_error(
      ejecutar_insumos_prueba(entradas, raiz),
      "ya existen"
    )
  }
)

testthat::test_that("las entregas canonicas deben coincidir", {
  raiz <- tempfile("insumos_snapshot_")
  dir.create(raiz)
  entradas <- crear_entradas_insumos(raiz, alterar_snapshot = TRUE)
  testthat::expect_error(
    ejecutar_insumos_prueba(entradas, raiz),
    "misma entrega canonica"
  )
})
