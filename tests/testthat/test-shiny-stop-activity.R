source(file.path("..", "..", "Shiny", "R", "consultas_temporales.R"))
source(file.path("..", "..", "Shiny", "R", "actividad_paradas.R"))
testthat::test_that("paradas compartidas conservan movimientos y buses sin duplicados", {
  library(data.table)
  b <- data.table(recorrido="A",COD_VARIAN="V",seqViajeN=1:3,tiempo=c(100,200,300),buses=1L)
  index <- data.table(COD_VARIAN="V",seqViajeN=c(1,1,1,3),COD_UBIC_P=c(10,10,11,30))
  d <- data.table(recorrido="A",seq_origen_expansion=c(1,1),seq_destino_expansion=c(3,1),
    COD_PARADA_ORIGEN_EXPANSION=c(10,11),COD_PARADA_DESTINO_EXPANSION=c(30,11),cantidad_pasajeros=c(2,3))
  r <- build_stop_activity(b,index,d)
  testthat::expect_equal(nrow(r),3L)
  testthat::expect_equal(r[COD_UBIC_P==10,subidas],2)
  testthat::expect_equal(r[COD_UBIC_P==11,subidas],3)
  testthat::expect_equal(r[COD_UBIC_P==11,bajadas],3)
  testthat::expect_equal(r[COD_UBIC_P==30,bajadas],2)
  testthat::expect_true(all(r$buses==1))
})
testthat::test_that("horarios y lineas filtran pasadas antes de agregar", {
  library(data.table)
  t <- as.numeric(as.POSIXct("2022-07-08",tz="America/Montevideo"))
  d <- list(stop_passes=data.table(COD_UBIC_P=1,COD_LINEA=c("A","B","A"),
    tipo_servicio=c("urbano","inter","urbano"),
    tiempo=t+c(7,8,10)*3600,subidas=c(2,3,8),bajadas=0,buses=1),
    stops=data.table(COD_UBIC_P=1,lon=0,lat=0))
  r <- query_stop_activity(d,"2022-07-08",c(7,10),"A")
  testthat::expect_equal(r$subidas,2)
  testthat::expect_equal(r$buses,1)
  inter <- query_stop_activity(
    d, "2022-07-08", c(7,10), service_type="inter"
  )
  testthat::expect_equal(inter$subidas,3)
  testthat::expect_equal(inter$buses,1)
})

testthat::test_that("concentracion elige el prefijo minimo e incluye empates", {
  library(data.table)
  stops <- data.table(COD_UBIC_P=1:4,CALLE=letters[1:4],subidas=c(40,30,20,10),bajadas=0,buses=1)
  result <- calculate_stop_concentration(stops,"subidas",.7)
  testthat::expect_equal(nrow(result),4L)
  testthat::expect_equal(sum(result$incluida),2L)
  testthat::expect_equal(max(result[incluida == TRUE,porcentaje_acumulado]),.7)
  ties <- calculate_stop_concentration(data.table(COD_UBIC_P=1:3,subidas=c(40,30,30),bajadas=0,buses=1),"subidas",.6)
  testthat::expect_equal(sum(ties$incluida),3L)
  testthat::expect_equal(max(ties[incluida == TRUE,porcentaje_acumulado]),1)
  testthat::expect_error(calculate_stop_concentration(stops,"subidas",1.1))
  summary <- summarize_stop_concentration(result)
  testthat::expect_equal(summary$visible,2L)
  testthat::expect_equal(summary$active,4L)
  testthat::expect_equal(summary$stop_percentage,50)
  testthat::expect_equal(summary$captured_percentage,70)
})

testthat::test_that("actividad de extremos combina subidas y bajadas por parada", {
  library(data.table)
  origin <- data.table(COD_UBIC_P=c(1,2),personas=c(10,5))
  destination <- data.table(COD_UBIC_P=c(2,3),personas=c(4,11))
  stops <- data.table(COD_UBIC_P=1:3,CALLE=letters[1:3],lon=1:3,lat=1:3)
  result <- combine_endpoint_stop_activity(origin,destination,stops)
  testthat::expect_equal(result$subidas,c(10,5,0))
  testthat::expect_equal(result$bajadas,c(0,4,11))
  testthat::expect_equal(result$CALLE,letters[1:3])
})

