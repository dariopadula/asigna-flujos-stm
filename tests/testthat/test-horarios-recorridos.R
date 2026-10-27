source(file.path(
  "..", "..", "pipeline_diario", "R", "horarios",
  "estimar_horarios_recorridos.R"
))

crear_entradas_horarios <- function(raiz, alterar_snapshot = FALSE) {
  fecha <- "2026-05-20"
  metadatos <- list(
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1"
  )
  escribir_manifiesto_diario <- function(directorio, snapshot = NULL) {
    snapshot_elegido <- if (is.null(snapshot)) {
      metadatos$snapshot_referencias
    } else {
      snapshot
    }
    yaml::write_yaml(list(
      fecha_operativa = fecha,
      version_registro_variantes = metadatos$version_registro_variantes,
      snapshot_referencias = snapshot_elegido,
      red_canonica = metadatos$red_canonica
    ), file.path(directorio, "manifiesto.yml"))
  }

  od <- data.table::data.table(
    trip_id = 1:5,
    cod_empresa = "1",
    numero_evento_real_subida = c("100", "100", "200", "300", "400"),
    fecha_evento_subida = c(
      "2026-05-20 10:00:00", "2026-05-20 10:00:10",
      "2026-05-20 11:00:00", NA_character_, "2026-05-20 12:00:00"
    ),
    fecha_evento_bajada = c(
      "2026-05-20 10:01:40", "2026-05-20 10:01:30",
      "2026-05-20 11:00:01", NA_character_, "2026-05-20 12:02:00"
    ),
    cantidad_pasajeros = 1,
    tipo_servicio = "urbano"
  )
  archivo_od <- file.path(raiz, "od_operativa.parquet")
  arrow::write_parquet(od, archivo_od)

  paso09 <- file.path(raiz, "paso09")
  dir.create(paso09)
  resolucion <- data.table::data.table(
    trip_id = 1:5,
    id_version_variante = "A_1",
    indice_secuencia_subida = c(1L, NA_integer_, 1L, 1L, 1L),
    indice_secuencia_bajada = c(3L, NA_integer_, 3L, 3L, 3L),
    punto_canonico_subida = c(
      "P1", NA_character_, "P1", "P1", "P1"
    ),
    punto_canonico_bajada = c(
      "P3", NA_character_, "P3", "P3", "P3"
    ),
    estado_resolucion = c(
      "apta_secuencia", "variante_subida_bajada_distinta",
      "apta_secuencia", "apta_secuencia", "apta_secuencia"
    )
  )
  arrow::write_parquet(
    resolucion, file.path(paso09, "resolucion_paradas.parquet")
  )
  escribir_manifiesto_diario(paso09)

  paso10 <- file.path(raiz, "paso10")
  dir.create(paso10)
  recuperacion <- data.table::data.table(
    trip_id = 2L,
    id_version_variante_subida = "A_1",
    indice_secuencia_subida = 1L,
    punto_canonico_subida = "P1",
    indice_secuencia_bajada_asignado = 3L,
    punto_canonico_bajada_asignado = "P3",
    recuperada = TRUE
  )
  arrow::write_parquet(
    recuperacion, file.path(paso10, "recuperaciones_paradas.parquet")
  )
  escribir_manifiesto_diario(paso10)

  paso11 <- file.path(raiz, "paso11")
  dir.create(paso11)
  recorridos <- data.table::data.table(
    recorrido = paste(
      fecha, "1", c("100", "200", "300", "400"), sep = "|"
    ),
    id_version_variante = "A_1",
    tipo_servicio = "urbano",
    registros_utilizables = c(2L, 1L, 1L, 1L),
    personas_utilizables = c(2, 1, 1, 1),
    registros_recuperados = c(1L, 0L, 0L, 0L)
  )
  arrow::write_parquet(
    recorridos, file.path(paso11, "recorridos_operativos.parquet")
  )
  escribir_manifiesto_diario(
    paso11,
    if (alterar_snapshot) "otro_snapshot" else NULL
  )

  registro <- file.path(raiz, "registro")
  dir.create(registro)
  secuencias <- data.table::data.table(
    id_version_variante = "A_1",
    indice_secuencia = 1:3,
    punto_canonico_id = paste0("P", 1:3),
    x_canonica = c(0, 100, 200),
    y_canonica = c(0, 0, 0)
  )
  arrow::write_parquet(
    secuencias, file.path(registro, "secuencias.parquet")
  )
  yaml::write_yaml(list(
    version = metadatos$version_registro_variantes,
    snapshot_referencias = metadatos$snapshot_referencias,
    red_canonica = metadatos$red_canonica
  ), file.path(registro, "manifiesto.yml"))

  list(
    fecha = fecha,
    od = archivo_od,
    paso09 = paso09,
    paso10 = paso10,
    paso11 = paso11,
    registro = registro
  )
}

testthat::test_that("se estiman horarios por lotes sin duplicar los viajes", {
  raiz <- tempfile("horarios_recorridos_")
  dir.create(raiz)
  entradas <- crear_entradas_horarios(raiz)
  resultado <- estimar_horarios_recorridos(
    entradas$fecha, entradas$od, entradas$paso09, entradas$paso10,
    entradas$paso11, entradas$registro, file.path(raiz, "intermediate"),
    recorridos_por_lote = 1L
  )

  archivos_pasadas <- list.files(
    file.path(resultado$destino, "pasadas_bus"),
    pattern = "[.]parquet$", full.names = TRUE
  )
  pasadas <- data.table::rbindlist(lapply(
    archivos_pasadas,
    function(x) data.table::as.data.table(arrow::read_parquet(x))
  ))

  testthat::expect_length(archivos_pasadas, 4L)
  testthat::expect_equal(nrow(pasadas), 6L)
  testthat::expect_setequal(
    unique(pasadas$recorrido),
    c("2026-05-20|1|100", "2026-05-20|1|400")
  )
  testthat::expect_true(all(
    pasadas[, all(diff(tiempo) >= 0), by = recorrido]$V1
  ))
  testthat::expect_true(all(
    pasadas[, identical(indice_secuencia, 1:3), by = recorrido]$V1
  ))
  testthat::expect_setequal(
    resultado$calidad$estado_horario,
    c(
      "horario_completo", "revision_temporal_velocidad", "sin_anclas"
    )
  )
  testthat::expect_equal(nrow(resultado$cuarentena), 2L)
  testthat::expect_true(all(resultado$controles$aprobado))
  testthat::expect_equal(sum(resultado$controles$aprobado), 13L)
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "manifiesto.yml"
  )))
  testthat::expect_false(file.exists(file.path(
    resultado$destino, "viajes_recorridos.parquet"
  )))
  testthat::expect_false(dir.exists(file.path(
    dirname(resultado$destino), ".checkpoint_fecha=20260520"
  )))
  testthat::expect_error(
    estimar_horarios_recorridos(
      entradas$fecha, entradas$od, entradas$paso09, entradas$paso10,
      entradas$paso11, entradas$registro,
      file.path(raiz, "intermediate"), recorridos_por_lote = 1L
    ),
    "ya existen"
  )
})

testthat::test_that("las entregas canonicas deben coincidir", {
  raiz <- tempfile("horarios_snapshot_")
  dir.create(raiz)
  entradas <- crear_entradas_horarios(raiz, alterar_snapshot = TRUE)
  testthat::expect_error(
    estimar_horarios_recorridos(
      entradas$fecha, entradas$od, entradas$paso09, entradas$paso10,
      entradas$paso11, entradas$registro, file.path(raiz, "intermediate")
    ),
    "misma entrega"
  )
})

testthat::test_that("las anclas se ajustan y proyectan monotonamente", {
  observaciones <- data.table::data.table(
    distancia_m = c(0, 100, 200),
    tiempo = c(1000, 995, 1100),
    fuente = c("subida", "subida", "bajada")
  )
  anclas <- preparar_anclas_horario(observaciones)
  estimacion <- estimar_tiempos_secuencia(
    c(0, 50, 100, 150, 200), anclas, velocidad_respaldo = 5
  )

  testthat::expect_true(all(diff(anclas$tiempo) >= 0))
  testthat::expect_true(max(abs(anclas$ajuste_s)) > 0)
  testthat::expect_true(all(is.finite(estimacion$tiempo)))
  testthat::expect_true(all(diff(estimacion$tiempo) >= 0))
  testthat::expect_true("interpolado" %in% estimacion$metodo_tiempo)
})
