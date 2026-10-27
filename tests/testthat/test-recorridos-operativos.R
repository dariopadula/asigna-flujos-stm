source(file.path(
  "..", "..", "pipeline_diario", "R", "recorridos",
  "construir_recorridos_operativos.R"
))

crear_entradas_recorridos_operativos <- function(
    raiz, alterar_snapshot = FALSE) {
  fecha <- "2026-05-20"
  od <- data.table::data.table(
    trip_id = 1:10,
    cod_empresa = "1",
    numero_evento_real_subida = c(
      "100", "100", "100", "200", "200",
      "300", "300", "400", "600", NA_character_
    ),
    cantidad_pasajeros = 1,
    tipo_servicio = c(
      "urbano", "urbano", "urbano", "urbano", "urbano",
      "urbano", "inter", "urbano", "urbano", "urbano"
    )
  )
  archivo_od <- file.path(raiz, "od_operativa.parquet")
  arrow::write_parquet(od, archivo_od)

  directorio_paso09 <- file.path(raiz, "paso09")
  dir.create(directorio_paso09)
  estados <- c(
    "apta_secuencia", "apta_secuencia",
    "variante_subida_bajada_distinta",
    "apta_secuencia", "apta_secuencia", "apta_secuencia",
    "apta_secuencia", "sin_variante_canonica",
    "variante_subida_bajada_distinta", "apta_secuencia"
  )
  variantes <- c("A_1", "A_1", "A_1", "A_1", "B_1", "A_1", "A_1", NA, "A_1", "A_1")
  aptas <- estados == "apta_secuencia"
  paso09 <- data.table::data.table(
    trip_id = 1:10,
    id_version_variante = variantes,
    indice_secuencia_subida = data.table::fifelse(aptas, 1L, NA_integer_),
    indice_secuencia_bajada = data.table::fifelse(aptas, 5L, NA_integer_),
    punto_canonico_subida = data.table::fifelse(aptas, "P1", NA_character_),
    punto_canonico_bajada = data.table::fifelse(aptas, "P5", NA_character_),
    estado_resolucion = estados
  )
  arrow::write_parquet(
    paso09, file.path(directorio_paso09, "resolucion_paradas.parquet")
  )
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = "referencias_prueba",
    red_canonica = "canonica_prueba_v1"
  ), file.path(directorio_paso09, "manifiesto.yml"))

  directorio_paso10 <- file.path(raiz, "paso10")
  dir.create(directorio_paso10)
  paso10 <- data.table::data.table(
    trip_id = c(3L, 9L),
    id_version_variante_subida = c("A_1", "A_1"),
    indice_secuencia_subida = c(2L, 2L),
    punto_canonico_subida = c("P2", "P2"),
    indice_secuencia_bajada_asignado = c(6L, NA_integer_),
    punto_canonico_bajada_asignado = c("P6", NA_character_),
    recuperada = c(TRUE, FALSE),
    requiere_revision = c(FALSE, FALSE),
    accion_resolucion = c(
      "parada_exacta_en_variante_subida", "distancia_mayor_umbral"
    )
  )
  arrow::write_parquet(
    paso10, file.path(directorio_paso10, "recuperaciones_paradas.parquet")
  )
  yaml::write_yaml(list(
    fecha_operativa = fecha,
    version_registro_variantes = "variantes_prueba_v1",
    snapshot_referencias = if (alterar_snapshot) {
      "otro_snapshot"
    } else {
      "referencias_prueba"
    },
    red_canonica = "canonica_prueba_v1"
  ), file.path(directorio_paso10, "manifiesto.yml"))

  list(
    fecha = fecha,
    od = archivo_od,
    paso09 = directorio_paso09,
    paso10 = directorio_paso10
  )
}

testthat::test_that("se construye un catalogo liviano por recorrido", {
  raiz <- tempfile("recorridos_operativos_")
  dir.create(raiz)
  entradas <- crear_entradas_recorridos_operativos(raiz)
  resultado <- construir_recorridos_operativos(
    entradas$fecha, entradas$od, entradas$paso09, entradas$paso10,
    file.path(raiz, "intermediate")
  )

  testthat::expect_equal(nrow(resultado$recorridos), 1L)
  testthat::expect_equal(
    resultado$recorridos$recorrido, "2026-05-20|1|100"
  )
  testthat::expect_equal(resultado$recorridos$id_version_variante, "A_1")
  testthat::expect_equal(resultado$recorridos$registros_aptos_base, 2L)
  testthat::expect_equal(resultado$recorridos$registros_recuperados, 1L)
  testthat::expect_equal(resultado$recorridos$registros_utilizables, 3L)
  testthat::expect_setequal(
    resultado$cuarentena$estado_recorrido,
    c(
      "variantes_ambiguas", "tipos_servicio_ambiguos",
      "sin_variante_canonica", "sin_viajes_elegibles",
      "clave_recorrido_invalida"
    )
  )
  testthat::expect_true(all(resultado$controles$aprobado))
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "recorridos_operativos.parquet"
  )))
  testthat::expect_true(file.exists(file.path(
    resultado$destino, "recorridos_cuarentena.csv"
  )))
  testthat::expect_false(file.exists(file.path(
    resultado$destino, "viajes_recorridos.parquet"
  )))
  testthat::expect_error(
    construir_recorridos_operativos(
      entradas$fecha, entradas$od, entradas$paso09, entradas$paso10,
      file.path(raiz, "intermediate")
    ),
    "ya existen"
  )
})

testthat::test_that("los pasos 09 y 10 deben usar la misma entrega", {
  raiz <- tempfile("recorridos_snapshot_")
  dir.create(raiz)
  entradas <- crear_entradas_recorridos_operativos(
    raiz, alterar_snapshot = TRUE
  )
  testthat::expect_error(
    construir_recorridos_operativos(
      entradas$fecha, entradas$od, entradas$paso09, entradas$paso10,
      file.path(raiz, "intermediate")
    ),
    "misma entrega"
  )
})
