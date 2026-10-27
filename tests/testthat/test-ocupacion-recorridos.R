source(file.path(
  "..", "..", "pipeline_diario", "R", "ocupacion",
  "calcular_ocupacion_recorridos.R"
))

crear_entradas_ocupacion <- function(raiz, alterar_snapshot = FALSE) {
  fecha <- "2026-05-20"
  metadatos <- list(
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1"
  )
  escribir_manifiesto <- function(directorio, snapshot = NULL) {
    yaml::write_yaml(list(
      fecha_operativa = fecha,
      version_registro_variantes = metadatos$version_registro_variantes,
      snapshot_referencias = if (is.null(snapshot)) {
        metadatos$snapshot_referencias
      } else {
        snapshot
      },
      red_canonica = metadatos$red_canonica
    ), file.path(directorio, "manifiesto.yml"))
  }

  od <- data.table::data.table(
    trip_id = 1:6,
    cod_empresa = "1",
    numero_evento_real_subida = c(
      "100", "100", "200", "300", "400", "500"
    ),
    fecha_evento_subida = c(
      "2026-05-20 10:00:00", "2026-05-20 10:20:00",
      "2026-05-20 11:00:00", NA_character_,
      "2026-05-20 12:00:00", "2026-05-20 13:00:00"
    ),
    fecha_evento_bajada = c(
      "2026-05-20 10:10:00", "2026-05-20 10:10:00",
      "2026-05-20 11:10:00", NA_character_,
      "2026-05-20 12:10:00", "2026-05-20 13:10:00"
    ),
    cantidad_pasajeros = c(2, 1, 3, 2, 1, 4),
    tipo_servicio = "urbano"
  )
  archivo_od <- file.path(raiz, "od_operativa.parquet")
  arrow::write_parquet(od, archivo_od)

  paso09 <- file.path(raiz, "paso09")
  dir.create(paso09)
  resolucion <- data.table::data.table(
    trip_id = 1:6,
    id_version_variante = "A_1",
    indice_secuencia_subida = c(1L, 2L, 1L, 2L, 1L, 1L),
    indice_secuencia_bajada = c(3L, 4L, NA_integer_, 4L, 4L, 4L),
    punto_canonico_subida = c("P1", "P2", "P1", "P2", "P1", "P1"),
    punto_canonico_bajada = c(
      "P3", "P4", NA_character_, "P4", "P4", "P4"
    ),
    estado_resolucion = c(
      "apta_secuencia", "apta_secuencia",
      "variante_subida_bajada_distinta", "apta_secuencia",
      "apta_secuencia", "apta_secuencia"
    )
  )
  arrow::write_parquet(
    resolucion, file.path(paso09, "resolucion_paradas.parquet")
  )
  escribir_manifiesto(paso09)

  paso10 <- file.path(raiz, "paso10")
  dir.create(paso10)
  recuperacion <- data.table::data.table(
    trip_id = 3L,
    id_version_variante_subida = "A_1",
    indice_secuencia_subida = 1L,
    punto_canonico_subida = "P1",
    indice_secuencia_bajada_asignado = 4L,
    punto_canonico_bajada_asignado = "P4",
    recuperada = TRUE,
    requiere_revision = TRUE,
    accion_resolucion = "parada_cercana_en_variante_subida"
  )
  arrow::write_parquet(
    recuperacion, file.path(paso10, "recuperaciones_paradas.parquet")
  )
  escribir_manifiesto(paso10)

  paso11 <- file.path(raiz, "paso11")
  dir.create(paso11)
  recorridos <- data.table::data.table(
    recorrido = paste(
      fecha, "1", c("100", "200", "300", "400", "500"), sep = "|"
    ),
    id_version_variante = "A_1",
    tipo_servicio = "urbano",
    registros_utilizables = c(2L, 1L, 1L, 1L, 1L),
    personas_utilizables = c(3, 3, 2, 1, 4),
    registros_recuperados = c(0L, 1L, 0L, 0L, 0L)
  )
  arrow::write_parquet(
    recorridos, file.path(paso11, "recorridos_operativos.parquet")
  )
  escribir_manifiesto(paso11)

  paso12 <- file.path(raiz, "paso12")
  dir.create(file.path(paso12, "pasadas_bus"), recursive = TRUE)
  dir.create(file.path(paso12, "calidad_recorridos"), recursive = TRUE)
  claves <- recorridos$recorrido
  crear_pasadas <- function(recorridos_lote, inicio_tiempo) {
    data.table::rbindlist(lapply(seq_along(recorridos_lote), function(i) {
      data.table::data.table(
        recorrido = recorridos_lote[[i]],
        id_version_variante = "A_1",
        tipo_servicio = "urbano",
        indice_secuencia = 1:4,
        punto_canonico_id = paste0("P", 1:4),
        distancia_m = c(0, 100, 200, 300),
        tiempo = inicio_tiempo + i * 100 + 0:3 * 20,
        metodo_tiempo = "ancla_resumida",
        buses = 1L
      )
    }))
  }
  arrow::write_parquet(
    crear_pasadas(claves[1:2], 1000),
    file.path(paso12, "pasadas_bus", "part-00001.parquet")
  )
  pasadas_2 <- crear_pasadas(claves[c(3, 5)], 2000)
  pasadas_2[
    recorrido == claves[5],
    tiempo := c(1000, 1000 + 20 * 3600, 1000 + 40 * 3600, 1000 + 50 * 3600)
  ]
  arrow::write_parquet(
    pasadas_2,
    file.path(paso12, "pasadas_bus", "part-00002.parquet")
  )
  calidad_1 <- data.table::data.table(
    recorrido = claves[c(1, 2, 4)],
    id_version_variante = "A_1",
    tipo_servicio = "urbano",
    estado_horario = c(
      "horario_completo", "horario_completo",
      "revision_temporal_ajuste"
    )
  )
  calidad_2 <- data.table::data.table(
    recorrido = claves[c(3, 5)],
    id_version_variante = "A_1",
    tipo_servicio = "urbano",
    estado_horario = "horario_completo"
  )
  arrow::write_parquet(
    calidad_1,
    file.path(paso12, "calidad_recorridos", "part-00001.parquet")
  )
  arrow::write_parquet(
    calidad_2,
    file.path(paso12, "calidad_recorridos", "part-00002.parquet")
  )
  escribir_manifiesto(
    paso12,
    if (alterar_snapshot) "otro_snapshot" else NULL
  )

  list(
    fecha = fecha,
    od = archivo_od,
    paso09 = paso09,
    paso10 = paso10,
    paso11 = paso11,
    paso12 = paso12
  )
}

testthat::test_that(
  "se calcula ocupacion por lotes y se aislan tiempos invertidos",
  {
    raiz <- tempfile("ocupacion_recorridos_")
    dir.create(raiz)
    entradas <- crear_entradas_ocupacion(raiz)
    resultado <- calcular_ocupacion_recorridos(
      entradas$fecha,
      entradas$od,
      entradas$paso09,
      entradas$paso10,
      entradas$paso11,
      entradas$paso12,
      file.path(raiz, "intermediate")
    )

    archivos <- list.files(
      file.path(resultado$destino, "pasadas_ocupacion"),
      pattern = "[.]parquet$", full.names = TRUE
    )
    ocupacion <- data.table::rbindlist(lapply(
      archivos,
      function(x) data.table::as.data.table(arrow::read_parquet(x))
    ))
    data.table::setorder(ocupacion, recorrido, indice_secuencia)

    testthat::expect_length(archivos, 2L)
    testthat::expect_equal(nrow(ocupacion), 12L)
    testthat::expect_setequal(
      unique(ocupacion$recorrido),
      paste(entradas$fecha, "1", c("100", "200", "300"), sep = "|")
    )
    testthat::expect_equal(
      ocupacion[recorrido == paste(entradas$fecha, "1", "100", sep = "|"),
        ocupacion_personas],
      c(2, 2, 0, 0)
    )
    testthat::expect_equal(
      ocupacion[recorrido == paste(entradas$fecha, "1", "200", sep = "|"),
        ocupacion_personas],
      c(3, 3, 3, 0)
    )
    testthat::expect_equal(
      ocupacion[recorrido == paste(entradas$fecha, "1", "300", sep = "|"),
        ocupacion_personas],
      c(0, 2, 2, 0)
    )
    testthat::expect_equal(sum(ocupacion$subidas), 7)
    testthat::expect_equal(sum(ocupacion$bajadas), 7)
    testthat::expect_equal(sum(ocupacion$ocupacion_personas), 17)
    testthat::expect_equal(resultado$cuarentena_tiempo$trip_id, 2L)
    testthat::expect_equal(
      resultado$cuarentena_duracion$recorrido,
      paste(entradas$fecha, "1", "500", sep = "|")
    )
    testthat::expect_equal(resultado$cuarentena_duracion$duracion_h, 50)
    testthat::expect_equal(resultado$cuarentena_duracion$registros_od, 1L)
    testthat::expect_equal(resultado$cuarentena_duracion$personas, 4)
    testthat::expect_equal(
      sum(resultado$calidad$registros_ocupacion), 3L
    )
    testthat::expect_true(all(resultado$controles$aprobado))
    testthat::expect_equal(sum(resultado$controles$aprobado), 17L)
    testthat::expect_true(file.exists(file.path(
      resultado$destino, "viajes_cuarentena_tiempo_invertido.parquet"
    )))
    testthat::expect_true(file.exists(file.path(
      resultado$destino, "recorridos_cuarentena_duracion.parquet"
    )))
    testthat::expect_false(file.exists(file.path(
      resultado$destino, "viajes_ocupacion.parquet"
    )))
    testthat::expect_false(dir.exists(file.path(
      dirname(resultado$destino), ".checkpoint_fecha=20260520"
    )))
    testthat::expect_error(
      calcular_ocupacion_recorridos(
        entradas$fecha,
        entradas$od,
        entradas$paso09,
        entradas$paso10,
        entradas$paso11,
        entradas$paso12,
        file.path(raiz, "intermediate")
      ),
      "ya existe"
    )
  }
)

testthat::test_that("las entregas de los pasos 09 a 12 deben coincidir", {
  raiz <- tempfile("ocupacion_snapshot_")
  dir.create(raiz)
  entradas <- crear_entradas_ocupacion(raiz, alterar_snapshot = TRUE)
  testthat::expect_error(
    calcular_ocupacion_recorridos(
      entradas$fecha,
      entradas$od,
      entradas$paso09,
      entradas$paso10,
      entradas$paso11,
      entradas$paso12,
      file.path(raiz, "intermediate")
    ),
    "misma fecha y entrega"
  )
})

testthat::test_that("los extremos deben existir en las pasadas", {
  pasadas <- data.table::data.table(
    recorrido = "R1",
    id_version_variante = "A_1",
    tipo_servicio = "urbano",
    indice_secuencia = 1:2,
    punto_canonico_id = c("P1", "P2"),
    distancia_m = c(0, 100),
    tiempo = c(1000, 1020),
    metodo_tiempo = "ancla_resumida",
    buses = 1L
  )
  viajes <- data.table::data.table(
    trip_id = 1L,
    recorrido = "R1",
    indice_subida_efectivo = 1L,
    indice_bajada_efectivo = 3L,
    cantidad_pasajeros = 1
  )
  testthat::expect_error(
    calcular_ocupacion_lote(pasadas, viajes),
    "fuera de las pasadas"
  )
})
