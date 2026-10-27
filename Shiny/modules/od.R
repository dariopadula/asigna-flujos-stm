odUI <- function(id, data) {
  ns <- shiny::NS(id)
  zone_choices <- c("Departamentos"="departamento","Barrios" = "barrio", "Municipios" = "municipio", "CCZ" = "ccz", "Dibujar polígono" = "poligono")
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(width = 255,class="od-sidebar",gap="0.55rem",padding="0.75rem",
      shiny::tags$div(class = "eyebrow", "CONSULTA ORIGEN–DESTINO"),
      shiny::h4("¿Cómo viajan entre dos zonas?"),
      shiny::selectInput(ns("date"), "Fecha", choices = as.character(data$dates), selected = as.character(data$dates[1])),
      shiny::radioButtons(ns("time_reference"), "Aplicar horario a", inline = TRUE,
        choices = c("Subida" = "subida", "Bajada" = "bajada")),
      shiny::sliderInput(ns("hours"), "Franja horaria", min = 0, max = 24, value = c(7, 10), step = .25, post = " h",ticks=FALSE),
      shiny::hr(),
      shiny::strong("Origen"),
      shiny::radioButtons(ns("origin_type"), "Definir origen", choices = zone_choices,inline=TRUE),
      shiny::selectizeInput(ns("origin_zones"), "Zonas de origen", choices = data$zone_choices$barrio, multiple = TRUE,
        options = list(placeholder = "Seleccioná una o más zonas", plugins = list("remove_button"))),
      shiny::hr(),
      shiny::strong("Destino"),
      shiny::radioButtons(ns("destination_type"), "Definir destino", choices = zone_choices,inline=TRUE),
      shiny::selectizeInput(ns("destination_zones"), "Zonas de destino", choices = data$zone_choices$barrio, multiple = TRUE,
        options = list(placeholder = "Seleccioná una o más zonas", plugins = list("remove_button"))),
      shiny::radioButtons(ns("polygon_target"), "Asignar el próximo polígono a", inline = TRUE,
        choices = c("Origen" = "origin", "Destino" = "destination")),
      shiny::actionButton(ns("apply"), "Calcular flujo", class = "btn-primary w-100"),
      shiny::helpText("Si usás polígonos, elegí primero a qué extremo corresponde cada dibujo. Podés editarlos o borrarlos con las herramientas del mapa. La franja se aplica a la subida o a la bajada según la opción elegida.")),
    bslib::navset_card_tab(
      bslib::nav_panel("Mapa",
        shiny::uiOutput(ns("summary")),
        bslib::card(bslib::card_header(shiny::textOutput(ns("caption"))),
          leaflet::leafletOutput(ns("map"), height = "52vh"), shiny::textOutput(ns("status"))),
        bslib::layout_columns(
          bslib::card(bslib::card_header("Paradas de subida seleccionadas"), shiny::tableOutput(ns("origins"))),
          bslib::card(bslib::card_header("Paradas de bajada seleccionadas"), shiny::tableOutput(ns("destinations"))))),
      bslib::nav_panel("Concentración de paradas",
        stopConcentrationUI(ns("stop_concentration"),c(subidas="Subidas",bajadas="Bajadas")))
    )
  )
}

odServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    origin_polygon <- shiny::reactiveVal(NULL)
    destination_polygon <- shiny::reactiveVal(NULL)

    update_zones <- function(type, input_id) {
      choices <- if (identical(type, "poligono")) character() else data$zone_choices[[type]]
      shiny::updateSelectizeInput(session, input_id, choices = choices, selected = character(), server = TRUE)
    }
    shiny::observeEvent(input$origin_type, {
      update_zones(input$origin_type,"origin_zones")
      if (!identical(input$origin_type,"poligono")) {
        origin_polygon(NULL)
        leaflet::leafletProxy("map",session=session) |>
          leaflet::clearGroup("Dibujo origen") |> leaflet::clearGroup("Zona origen")
      }
    },ignoreInit=TRUE)
    shiny::observeEvent(input$destination_type, {
      update_zones(input$destination_type,"destination_zones")
      if (!identical(input$destination_type,"poligono")) {
        destination_polygon(NULL)
        leaflet::leafletProxy("map",session=session) |>
          leaflet::clearGroup("Dibujo destino") |> leaflet::clearGroup("Zona destino")
      }
    },ignoreInit=TRUE)
    shiny::observeEvent(input$map_draw_new_feature, {
      candidate <- polygon_feature_to_sf(input$map_draw_new_feature)
      if (is.null(candidate)) return()
      is_origin <- identical(input$polygon_target,"origin")
      if (is_origin) origin_polygon(candidate) else destination_polygon(candidate)
      group <- if (is_origin) "Zona origen" else "Zona destino"
      leaflet::leafletProxy("map",session=session) |> leaflet::clearGroup(group)
    })
    shiny::observeEvent(input$map_draw_edited_features, {
      candidate <- polygon_feature_to_sf(input$map_draw_edited_features)
      if (is.null(candidate)) return()
      if (identical(input$polygon_target,"origin")) {
        origin_polygon(candidate)
        leaflet::leafletProxy("map",session=session) |> leaflet::clearGroup("Zona origen")
      } else {
        destination_polygon(candidate)
        leaflet::leafletProxy("map",session=session) |> leaflet::clearGroup("Zona destino")
      }
    })
    shiny::observeEvent(input$map_draw_deleted_features, {
      if (identical(input$polygon_target,"origin")) {
        origin_polygon(NULL)
        leaflet::leafletProxy("map",session=session) |> leaflet::clearGroup("Zona origen")
      } else {
        destination_polygon(NULL)
        leaflet::leafletProxy("map",session=session) |> leaflet::clearGroup("Zona destino")
      }
    })

    applied <- shiny::eventReactive(input$apply, {
      shiny::req(input$date, input$hours, input$origin_type, input$destination_type, input$time_reference)
      shiny::validate(shiny::need(input$hours[2] > input$hours[1], "Elegí una franja válida."))
      selected_origin_polygon <- if (input$origin_type == "poligono") origin_polygon() else NULL
      selected_destination_polygon <- if (input$destination_type == "poligono") destination_polygon() else NULL
      origin_ids <- select_zone_stop_ids(data, input$origin_type, input$origin_zones, selected_origin_polygon)
      destination_ids <- select_zone_stop_ids(data, input$destination_type, input$destination_zones, selected_destination_polygon)
      shiny::validate(shiny::need(length(origin_ids) > 0, "Seleccioná una zona de origen o dibujá su polígono."))
      shiny::validate(shiny::need(length(destination_ids) > 0, "Seleccioná una zona de destino o dibujá su polígono."))
      start <- proc.time()[[3]]
      query_data <- get_beta_day_data(data,input$date)
      result <- query_od_flow(query_data, input$date, input$hours, origin_ids, destination_ids, input$time_reference)
      result$seconds <- proc.time()[[3]] - start
      result$origin <- result$origin
      result$destination <- result$destination
      result$origin_ids <- origin_ids
      result$destination_ids <- destination_ids
      result$origin_type <- input$origin_type
      result$destination_type <- input$destination_type
      result$origin_zones <- input$origin_zones
      result$destination_zones <- input$destination_zones
      result$origin_polygon <- selected_origin_polygon
      result$destination_polygon <- selected_destination_polygon
      result$time_reference <- input$time_reference
      result$hours <- input$hours
      result
    }, ignoreNULL = TRUE)
    stop_activity <- shiny::reactive({
      r <- applied()
      combine_endpoint_stop_activity(r$origin,r$destination,data$stops)
    })
    concentration <- stopConcentrationServer("stop_concentration",stop_activity,
      c(subidas="Subidas",bajadas="Bajadas"))

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        add_beta_base_tiles() |>
        leaflet::setView(lng = -56.17, lat = -34.87, zoom = 11) |>
        leaflet.extras::addDrawToolbar(targetGroup = "Dibujo origen", polylineOptions = FALSE,
          markerOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
          polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE,
            shapeOptions=leaflet.extras::drawShapeOptions(color="#39d3e6",fillColor="#39d3e6",
              fillOpacity=.08,weight=2)),
          rectangleOptions = leaflet.extras::drawRectangleOptions(
            shapeOptions=leaflet.extras::drawShapeOptions(color="#39d3e6",fillColor="#39d3e6",
              fillOpacity=.08,weight=2)),
          editOptions = leaflet.extras::editToolbarOptions(),
          singleFeature = TRUE) |>
        leaflet::addLayersControl(baseGroups = beta_base_groups(),
          overlayGroups = c("Flujo", "Subidas", "Bajadas", "Zona origen", "Zona destino"),
          options = leaflet::layersControlOptions(collapsed = FALSE))
    })

    zone_shape <- function(type, values) {
      if (type == "barrio") return(data$shapes$barrios[data$shapes$barrios$NOMBBARR %in% values, ])
      if (type == "municipio") return(data$shapes$municipio[data$shapes$municipio$MUNICIPIO %in% values, ])
      if (type == "ccz") return(data$shapes$ccz[data$shapes$ccz$CCZ %in% values, ])
      if (type == "departamento") return(data$shapes$departamento[data$shapes$departamento$DEPARTAMENTO %in% values, ])
      NULL
    }
    add_zone <- function(map, type, values, polygon, group, color) {
      if (type == "poligono") return(map)
      shape <- zone_shape(type, values)
      if (!has_spatial_features(shape)) return(map)
      leaflet::addPolygons(map, data = shape, color = color, weight = 2, fillOpacity = .08, group = group)
    }
    add_draw_toolbar <- function(map,target) {
      is_origin <- identical(target,"origin")
      group <- if(is_origin) "Dibujo origen" else "Dibujo destino"
      color <- if(is_origin) "#39d3e6" else "#ffc857"
      leaflet.extras::addDrawToolbar(map,targetGroup=group,polylineOptions=FALSE,
        markerOptions=FALSE,circleOptions=FALSE,circleMarkerOptions=FALSE,
        polygonOptions=leaflet.extras::drawPolygonOptions(showArea=TRUE,
          shapeOptions=leaflet.extras::drawShapeOptions(color=color,fillColor=color,
            fillOpacity=.08,weight=2)),
        rectangleOptions=leaflet.extras::drawRectangleOptions(
          shapeOptions=leaflet.extras::drawShapeOptions(color=color,fillColor=color,
            fillOpacity=.08,weight=2)),
        editOptions=leaflet.extras::editToolbarOptions(),singleFeature=TRUE)
    }
    shiny::observeEvent(input$polygon_target,{
      leaflet::leafletProxy("map",session=session) |>
        leaflet.extras::removeDrawToolbar(clearFeatures=FALSE) |>
        add_draw_toolbar(input$polygon_target)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$map_groups,{
      groups <- if(is.null(input$map_groups)) character() else input$map_groups
      proxy <- leaflet::leafletProxy("map",session=session)
      proxy <- if("Zona origen" %in% groups) leaflet::showGroup(proxy,"Dibujo origen") else leaflet::hideGroup(proxy,"Dibujo origen")
      if("Zona destino" %in% groups) leaflet::showGroup(proxy,"Dibujo destino") else leaflet::hideGroup(proxy,"Dibujo destino")
    },ignoreInit=TRUE)
    add_stops <- function(map, x, group, label) {
      if (!nrow(x)) return(map)
      visual <- beta_stop_visual(x$valor)
      pal <- beta_stop_palette(visual$top)
      leaflet::addCircleMarkers(map, lng = x$lon, lat = x$lat,
        radius = visual$radius, stroke = TRUE, weight = .7,
        color = "#17324d", opacity = .9, fillColor = pal(x$valor),
        fillOpacity = visual$opacity, group = group,
        popup = paste0("<b>", label, " ", x$COD_UBIC_P, "</b><br>", htmltools::htmlEscape(x$CALLE),
          "<br>Personas: ", format(x$valor, big.mark = ".", decimal.mark = ",")))
    }

    shiny::observeEvent(applied(), {
      r <- applied()
      proxy <- leaflet::leafletProxy("map", session = session) |>
        leaflet::clearGroup("Flujo") |>
        leaflet::clearGroup("Zona origen") |> leaflet::clearGroup("Zona destino") |>
        leaflet::removeControl("legend-od")
      if (nrow(r$flow)) {
        p <- r$flow
        top <- max(p$personas)
        pal <- leaflet::colorNumeric(c("#FFFFFF", "#FF0000"), domain = c(0, top))
        radius <- beta_flow_radius(p$personas)
        proxy <- proxy |> leaflet::addCircleMarkers(lng = p$lon, lat = p$lat,
          radius = radius, stroke = TRUE, weight = .35, color = "#7f0000",
          opacity = .8, fillColor = pal(p$personas),
          fillOpacity = .1 + .8 * p$personas / top, group = "Flujo",
          popup = paste0(htmltools::htmlEscape(p$NOM_CALLE),
            "<br>ID punto: ", htmltools::htmlEscape(p$ID_punto),
            "<br>Personas: ", format(p$personas, big.mark = ".", decimal.mark = ","))) |>
          leaflet::addLegend(position = "bottomright", pal = pal, values = c(0, top), title = "Flujo seleccionado",
            group = "Flujo", layerId = "legend-od")
      }
      proxy <- add_zone(proxy, r$origin_type, r$origin_zones, r$origin_polygon, "Zona origen", "#39d3e6")
      add_zone(proxy, r$destination_type, r$destination_zones, r$destination_polygon, "Zona destino", "#ffc857")
    })
    shiny::observeEvent(list(concentration$by_metric$subidas(),concentration$by_metric$bajadas()),{
      up <- concentration$by_metric$subidas()[incluida == TRUE]
      down <- concentration$by_metric$bajadas()[incluida == TRUE]
      proxy <- leaflet::leafletProxy("map",session=session) |>
        leaflet::clearGroup("Subidas") |> leaflet::clearGroup("Bajadas")
      proxy <- add_stops(proxy,up,"Subidas","Parada de subida")
      add_stops(proxy,down,"Bajadas","Parada de bajada")
    })

    output$summary <- shiny::renderUI({
      r <- applied()
      bslib::layout_columns(
        bslib::value_box("Personas entre zonas", format(sum(r$trips$cantidad_pasajeros), big.mark = ".", decimal.mark = ","), theme = "primary", height = "82px", fill = FALSE, class = "compact-value-box"),
        bslib::value_box("Registros OD", format(nrow(r$trips), big.mark = ".", decimal.mark = ","), theme = "light", height = "82px", fill = FALSE, class = "compact-value-box"),
        bslib::value_box("Recorridos", format(data.table::uniqueN(r$trips$recorrido), big.mark = ".", decimal.mark = ","), theme = "light", height = "82px", fill = FALSE, class = "compact-value-box"))
    })
    output$caption <- shiny::renderText({
      r <- applied()
      reference <- if (r$time_reference == "subida") "subidas" else "bajadas"
      sprintf("Flujo origen–destino · %s entre %02d:%02d y %02d:%02d", reference,
        floor(r$hours[1]), round(r$hours[1] %% 1 * 60), floor(r$hours[2]), round(r$hours[2] %% 1 * 60))
    })
    output$status <- shiny::renderText({
      r <- applied()
      sprintf("%s paradas de origen · %s de destino · consulta %.2f s",
        format(length(r$origin_ids), big.mark = ".", decimal.mark = ","),
        format(length(r$destination_ids), big.mark = ".", decimal.mark = ","), r$seconds)
    })
    rank <- function(x) head(x[order(-personas), .(Parada = COD_UBIC_P, Calle = CALLE, Personas = personas)], 10)
    output$origins <- shiny::renderTable(rank(applied()$origin), striped = TRUE)
    output$destinations <- shiny::renderTable(rank(applied()$destination), striped = TRUE)
  })
}

