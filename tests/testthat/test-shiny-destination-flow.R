source(file.path("..", "..", "Shiny", "R", "ocupacion.R"))
source(file.path("..", "..", "Shiny", "R", "consultas_temporales.R"))
source(file.path("..", "..", "Shiny", "R", "flujos_extremos.R"))

testthat::test_that("seleccion por zona y poligono devuelve paradas", {
  library(data.table)
  d <- list(
    stops=data.table(COD_UBIC_P=c(1L,2L),lon=c(-56.2,-55),lat=c(-34.9,-34)),
    stop_regions=data.table(COD_UBIC_P=c(1L,2L),BARRIO=c("CENTRO","OTRO"),CCZ=c("CCZ1","CCZ2"),MUNICIPIO=c("B","A")))
  testthat::expect_equal(select_destination_stop_ids(d,"barrio","CENTRO"),1L)
  feature <- list(geometry=list(type="Polygon",coordinates=list(list(
    c(-56.3,-35),c(-56.1,-35),c(-56.1,-34.8),c(-56.3,-34.8),c(-56.3,-35)))))
  polygon <- polygon_feature_to_sf(feature)
  collection <- list(type="FeatureCollection",features=list(feature))
  testthat::expect_true(has_spatial_features(polygon))
  testthat::expect_true(has_spatial_features(polygon_feature_to_sf(collection)))
  testthat::expect_false(has_spatial_features(NULL))
  testthat::expect_equal(select_destination_stop_ids(d,"poligono",polygon=polygon),1L)
})

testthat::test_that("seleccion por departamento usa la clave regional", {
  library(data.table)
  d <- list(stop_regions=data.table(
    COD_UBIC_P=1:3,DEPARTAMENTO=c("MONTEVIDEO","CANELONES",NA_character_)
  ))
  testthat::expect_equal(select_zone_stop_ids(d,"departamento","CANELONES"),2L)
})

testthat::test_that("destino filtra por hora de bajada y calcula flujo sin expansion", {
  library(data.table)
  start <- as.numeric(as.POSIXct("2022-07-08",tz="America/Montevideo"))
  d <- list(
    trips=data.table(trip_id=1:2,recorrido=c("A","A"),tiempo_bajada=start+c(8,11)*3600,
      COD_PARADA_ORIGEN_EXPANSION=c(1L,2L),COD_PARADA_DESTINO_EXPANSION=c(3L,3L),
      seq_origen_expansion=c(1,2),seq_destino_expansion=c(3,3),cantidad_pasajeros=c(2,5)),
    bus=data.table(recorrido="A",seqViajeN=1:3,ID_punto=paste0("p",1:3),buses=1),
    points=data.table(ID_punto=paste0("p",1:3),lon=1:3,lat=1:3),
    stops=data.table(COD_UBIC_P=1:3,lon=1:3,lat=1:3))
  r <- query_destination_flow(d,"2022-07-08",c(7,10),3L)
  testthat::expect_equal(sum(r$trips$cantidad_pasajeros),2)
  testthat::expect_equal(r$flow$personas,c(2,2))
  testthat::expect_equal(r$origin$COD_UBIC_P,1L)
  testthat::expect_equal(r$destination$COD_UBIC_P,3L)
})

testthat::test_that("origen filtra por parada y hora de subida", {
  library(data.table)
  start <- as.numeric(as.POSIXct("2022-07-08",tz="America/Montevideo"))
  d <- list(
    trips=data.table(trip_id=1:3,recorrido=c("A","A","A"),
      tiempo_subida=start+c(8,8,11)*3600,tiempo_bajada=start+c(9,9,12)*3600,
      COD_PARADA_ORIGEN_EXPANSION=c(1L,2L,1L),COD_PARADA_DESTINO_EXPANSION=c(3L,3L,3L),
      seq_origen_expansion=c(1,2,1),seq_destino_expansion=c(3,3,3),cantidad_pasajeros=c(2,5,7)),
    bus=data.table(recorrido="A",seqViajeN=1:3,ID_punto=paste0("p",1:3),buses=1),
    points=data.table(ID_punto=paste0("p",1:3),lon=1:3,lat=1:3),
    stops=data.table(COD_UBIC_P=1:3,lon=1:3,lat=1:3))
  r <- query_origin_flow(d,"2022-07-08",c(7,10),1L)
  testthat::expect_equal(sum(r$trips$cantidad_pasajeros),2)
  testthat::expect_equal(r$origin$COD_UBIC_P,1L)
  testthat::expect_equal(r$destination$COD_UBIC_P,3L)
})

testthat::test_that("origen destino combina ambos extremos y referencia horaria", {
  library(data.table)
  start <- as.numeric(as.POSIXct("2022-07-08",tz="America/Montevideo"))
  d <- list(
    trips=data.table(trip_id=1:3,recorrido=c("A","A","A"),
      tiempo_subida=start+c(8,8,6)*3600,tiempo_bajada=start+c(9,11,8)*3600,
      COD_PARADA_ORIGEN_EXPANSION=c(1L,1L,1L),COD_PARADA_DESTINO_EXPANSION=c(3L,3L,4L),
      seq_origen_expansion=c(1,1,1),seq_destino_expansion=c(3,3,3),cantidad_pasajeros=c(2,5,7)),
    bus=data.table(recorrido="A",seqViajeN=1:3,ID_punto=paste0("p",1:3),buses=1),
    points=data.table(ID_punto=paste0("p",1:3),lon=1:3,lat=1:3),
    stops=data.table(COD_UBIC_P=1:4,lon=1:4,lat=1:4))
  by_boarding <- query_od_flow(d,"2022-07-08",c(7,10),1L,3L,"subida")
  by_alighting <- query_od_flow(d,"2022-07-08",c(7,10),1L,3L,"bajada")
  testthat::expect_equal(sum(by_boarding$trips$cantidad_pasajeros),7)
  testthat::expect_equal(sum(by_alighting$trips$cantidad_pasajeros),2)
})

testthat::test_that("origen y destino filtran el otro extremo dentro o fuera de la zona", {
  library(data.table)
  start <- as.numeric(as.POSIXct("2022-07-08",tz="America/Montevideo"))
  d <- list(
    trips=data.table(trip_id=1:3,recorrido="A",
      tiempo_subida=start+8*3600,tiempo_bajada=start+9*3600,
      COD_PARADA_ORIGEN_EXPANSION=c(1L,1L,3L),
      COD_PARADA_DESTINO_EXPANSION=c(2L,3L,1L),
      seq_origen_expansion=1,seq_destino_expansion=3,
      cantidad_pasajeros=c(2,3,5)),
    bus=data.table(recorrido="A",seqViajeN=1:3,ID_punto=paste0("p",1:3),buses=1),
    points=data.table(ID_punto=paste0("p",1:3),lon=1:3,lat=1:3),
    stops=data.table(COD_UBIC_P=1:3,lon=1:3,lat=1:3))
  zone <- c(1L,2L)
  origin_all <- query_origin_flow(d,"2022-07-08",c(7,10),zone,"all")
  origin_inside <- query_origin_flow(d,"2022-07-08",c(7,10),zone,"inside")
  origin_outside <- query_origin_flow(d,"2022-07-08",c(7,10),zone,"outside")
  destination_all <- query_destination_flow(d,"2022-07-08",c(7,10),zone,"all")
  destination_inside <- query_destination_flow(d,"2022-07-08",c(7,10),zone,"inside")
  destination_outside <- query_destination_flow(d,"2022-07-08",c(7,10),zone,"outside")
  testthat::expect_equal(sum(origin_all$trips$cantidad_pasajeros),5)
  testthat::expect_equal(sum(origin_inside$trips$cantidad_pasajeros),2)
  testthat::expect_equal(sum(origin_outside$trips$cantidad_pasajeros),3)
  testthat::expect_equal(sum(destination_all$trips$cantidad_pasajeros),7)
  testthat::expect_equal(sum(destination_inside$trips$cantidad_pasajeros),2)
  testthat::expect_equal(sum(destination_outside$trips$cantidad_pasajeros),5)
})

