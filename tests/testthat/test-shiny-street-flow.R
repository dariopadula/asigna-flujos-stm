source(file.path("..", "..", "Shiny", "R", "ocupacion.R"))
source(file.path("..", "..", "Shiny", "R", "consultas_temporales.R"))
source(file.path("..", "..", "Shiny", "R", "consultas_flujo.R"))
source(file.path("..", "..", "Shiny", "R", "actividad_paradas.R"))

testthat::test_that("calles filtran por paso y distinguen tramo de recorrido asociado", {
  library(data.table)
  start <- as.numeric(as.POSIXct("2022-07-08",tz="America/Montevideo"))
  bus <- CJ(recorrido=c("A","B"),seqViajeN=1:4)
  bus[,`:=`(COD_LINEA="1",ID_punto=paste0("p",seqViajeN),
    tiempo=start+8*3600+seqViajeN*60,buses=1L,ocupacion_personas=0,
    metodo_tiempo="interpolado")]
  bus[,tipo_servicio:=ifelse(recorrido=="A","urbano","inter")]
  points <- data.table(ID_punto=paste0("p",1:4),NOM_CALLE=c("Inicio","Calle A","Calle B","Fin"),
    dirAv=c("N_S","N_S","E_O","N_S"),lon=1:4,lat=1:4)
  trips <- data.table(trip_id=1:3,recorrido=c("A","A","B"),
    tipo_servicio=c("urbano","urbano","inter"),
    seq_origen_expansion=c(1,3,1),seq_destino_expansion=c(4,4,2),cantidad_pasajeros=c(2,3,7),
    COD_PARADA_ORIGEN_EXPANSION=c("s1","s3","s1"),
    COD_PARADA_DESTINO_EXPANSION=c("s4","s4","s2"))
  bus <- calculate_occupancy(bus,trips)
  stop_passes <- bus[,.(recorrido,COD_UBIC_P=paste0("s",seqViajeN),
    buses,tipo_servicio)]
  stops <- data.table(COD_UBIC_P=paste0("s",1:4),CALLE=paste("Parada",1:4),lon=1:4,lat=1:4)
  data <- list(bus=bus,points=points,trips=trips,stop_passes=stop_passes,stops=stops)
  only <- query_street_flow(data,"2022-07-08",c(7,10),metric="personas",streets="Calle A")
  associated <- query_street_flow(data,"2022-07-08",c(7,10),metric="personas",streets="Calle A",display_mode="associated")
  both <- query_street_flow(data,"2022-07-08",c(7,10),metric="personas",
    streets=c("Calle A","Calle B"),display_mode="associated",match_mode="all")
  buses <- query_street_flow(data,"2022-07-08",c(7,10),metric="buses",streets="Calle A",display_mode="associated")
  urban <- query_street_flow(
    data,"2022-07-08",c(7,10),metric="buses",streets="Calle A",
    display_mode="associated",service_type="urbano"
  )
  inter <- query_street_flow(
    data,"2022-07-08",c(7,10),metric="buses",streets="Calle A",
    display_mode="associated",service_type="inter"
  )
  testthat::expect_equal(only$points$ID_punto,"p2")
  testthat::expect_equal(sum(associated$trips$cantidad_pasajeros),2)
  testthat::expect_equal(sum(both$trips$cantidad_pasajeros),2)
  testthat::expect_equal(buses$recorridos,2L)
  testthat::expect_equal(urban$recorridos,1L)
  testthat::expect_equal(inter$recorridos,1L)
  testthat::expect_true(all(urban$trips$tipo_servicio=="urbano"))
  testthat::expect_true(all(inter$trips$tipo_servicio=="inter"))
  activity <- query_street_stop_activity(data,associated)
  testthat::expect_equal(activity[COD_UBIC_P=="s1",subidas],2)
  testthat::expect_equal(activity[COD_UBIC_P=="s4",bajadas],2)
  testthat::expect_equal(sum(activity$buses),8)
  testthat::expect_false(any(activity$subidas==7))
  empty_activity <- query_street_stop_activity(data,
    list(eligible_runs=character(),trips=data.table()))
  testthat::expect_equal(nrow(empty_activity),0L)
  testthat::expect_true(all(c("subidas","bajadas","buses") %in% names(empty_activity)))
  testthat::expect_equal(select_street_points(data,"Calle A","E_O") |> nrow(),0L)
})

testthat::test_that("una clave visible agrupa segmentos homonimos", {
  library(data.table)
  data <- list(
    presentation_points=data.table(
      ID_presentacion=c("p1::N_S","p2::N_S"),ID_punto=c("p1","p2"),
      COD_NOMBRE=c("MVD_1","EXT_2"),NOM_CALLE="AV ITALIA",
      street_key="NAME::AV_ITALIA",dirAv="N_S",lon=1:2,lat=1:2
    )
  )

  selected <- select_street_points(data,"NAME::AV_ITALIA")

  testthat::expect_equal(nrow(selected),2L)
  testthat::expect_setequal(selected$COD_NOMBRE,c("MVD_1","EXT_2"))
})

