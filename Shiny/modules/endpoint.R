endpointConfig <- function(endpoint) {
  switch(endpoint,
    origin = list(
      eyebrow = "CONSULTA POR ORIGEN", title = "¿Hacia dónde van quienes salen de la zona?",
      date_label = "Fecha de salida", hour_label = "Hora de subida", zone_label = "Definir origen",
      help = "La franja filtra la hora de subida; el flujo muestra todo el trayecto de las personas elegidas.",
      value_label = "Personas que salen", caption = "Flujo desde origen", time_label = "subidas",
      zone_group = "Zona origen", zone_color = "#39d3e6",
      scope_label = "Destino de los viajes",
      scope_choices = c("Todos los destinos"="all","Fuera del origen seleccionado"="outside",
        "Dentro del origen seleccionado"="inside"),
      scope_caption = c(all="todos los destinos",outside="destino fuera de la zona",
        inside="destino dentro de la zona")),
    destination = list(
      eyebrow = "CONSULTA POR DESTINO", title = "¿Quiénes llegan a la zona?",
      date_label = "Fecha de llegada", hour_label = "Hora de bajada", zone_label = "Definir destino",
      help = "La franja filtra la hora de bajada; el flujo muestra todo el trayecto de las personas elegidas.",
      value_label = "Personas que llegan", caption = "Flujo hacia destino", time_label = "bajadas",
      zone_group = "Zona destino", zone_color = "#ffc857",
      scope_label = "Origen de los viajes",
      scope_choices = c("Todos los orígenes"="all","Fuera del destino seleccionado"="outside",
        "Dentro del destino seleccionado"="inside"),
      scope_caption = c(all="todos los orígenes",outside="origen fuera de la zona",
        inside="origen dentro de la zona")),
    stop("Extremo de consulta desconocido."))
}

endpointUI <- function(id, data, endpoint) {
  ns <- shiny::NS(id)
  config <- endpointConfig(endpoint)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(width = 270,
      shiny::tags$div(class = "eyebrow", config$eyebrow),
      shiny::h4(config$title),
      shiny::selectInput(ns("date"), config$date_label, choices = as.character(data$dates), selected = as.character(data$dates[1])),
      shiny::sliderInput(ns("hours"), config$hour_label, min = 0, max = 24, value = c(7, 10), step = .25, post = " h"),
      shiny::radioButtons(ns("zone_type"), config$zone_label,
        choices = c("Departamentos"="departamento","Barrios" = "barrio", "Municipios" = "municipio", "CCZ" = "ccz", "Dibujar polígono" = "poligono")),
      shiny::selectizeInput(ns("zones"), "Zonas", choices = data$zone_choices$barrio, multiple = TRUE,
        options = list(placeholder = "Seleccioná una o más zonas", plugins = list("remove_button"))),
      shiny::radioButtons(ns("other_scope"),config$scope_label,choices=config$scope_choices),
      shiny::actionButton(ns("apply"), "Calcular flujo", class = "btn-primary w-100"),
      shiny::hr(),
      shiny::helpText("Para un polígono, elegí Dibujar polígono y usá la herramienta del mapa. ", config$help)),
    bslib::navset_card_tab(
      bslib::nav_panel("Mapa",
        shiny::uiOutput(ns("summary")),
        bslib::card(bslib::card_header(shiny::textOutput(ns("caption"))),
          leaflet::leafletOutput(ns("map"), height = "66vh"), shiny::textOutput(ns("status"))),
        bslib::layout_columns(
          bslib::card(bslib::card_header("Principales paradas de subida"), shiny::tableOutput(ns("origins"))),
          bslib::card(bslib::card_header("Principales paradas de bajada"), shiny::tableOutput(ns("destinations"))))),
      bslib::nav_panel("Concentración de paradas",
        stopConcentrationUI(ns("stop_concentration"),c(subidas="Subidas",bajadas="Bajadas")))
    )
  )
}

destinationUI <- function(id, data) endpointUI(id, data, "destination")
originUI <- function(id, data) endpointUI(id, data, "origin")

endpointServer <- function(id, data, endpoint) {
  config <- endpointConfig(endpoint)
  shiny::moduleServer(id, function(input, output, session) {
    polygon <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$zone_type, {
      choices <- if (identical(input$zone_type, "poligono")) character() else data$zone_choices[[input$zone_type]]
      shiny::updateSelectizeInput(session, "zones", choices = choices, selected = character(), server = TRUE)
      if (!identical(input$zone_type,"poligono")) {
        polygon(NULL)
        leaflet::leafletProxy("map",session=session) |>
          leaflet::clearGroup("Dibujo de zona") |> leaflet::clearGroup(config$zone_group)
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$map_draw_new_feature, {
      candidate <- polygon_feature_to_sf(input$map_draw_new_feature)
      if (!is.null(candidate)) {
        polygon(candidate)
        leaflet::leafletProxy("map",session=session) |> leaflet::clearGroup(config$zone_group)
      }
    })
    shiny::observeEvent(input$map_draw_edited_features, {
      candidate <- polygon_feature_to_sf(input$map_draw_edited_features)
      if (!is.null(candidate)) {
        polygon(candidate)
        leaflet::leafletProxy("map",session=session) |> leaflet::clearGroup(config$zone_group)
      }
    })
    shiny::observeEvent(input$map_draw_deleted_features, {
      polygon(NULL)
      leaflet::leafletProxy("map",session=session) |> leaflet::clearGroup(config$zone_group)
    })

    applied <- shiny::eventReactive(input$apply, {
      shiny::req(input$date, input$hours, input$zone_type)
      shiny::validate(shiny::need(input$hours[2] > input$hours[1], "Elegí una franja válida."))
      chosen_polygon <- if (identical(input$zone_type, "poligono")) polygon() else NULL
      stop_ids <- select_zone_stop_ids(data, input$zone_type, input$zones, chosen_polygon)
      shiny::validate(shiny::need(length(stop_ids) > 0, "Seleccioná zonas o dibujá un polígono que contenga paradas."))
      start <- proc.time()[[3]]
      query_data <- get_beta_day_data(data,input$date)
      result <- if (endpoint == "origin") {
        query_origin_flow(query_data, input$date, input$hours, stop_ids, input$other_scope)
      } else {
        query_destination_flow(query_data, input$date, input$hours, stop_ids, input$other_scope)
      }
      result$seconds <- proc.time()[[3]] - start
      result$zone_type <- input$zone_type
      result$zones <- input$zones
      result$polygon <- chosen_polygon
      result$stop_ids <- stop_ids
      result$hours <- input$hours
      result$other_scope <- input$other_scope
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
        leaflet.extras::addDrawToolbar(targetGroup = "Dibujo de zona", polylineOptions = FALSE,
          markerOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
          polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE,
            shapeOptions=leaflet.extras::drawShapeOptions(color=config$zone_color,
              fillColor=config$zone_color,fillOpacity=.08,weight=2)),
          rectangleOptions = leaflet.extras::drawRectangleOptions(
            shapeOptions=leaflet.extras::drawShapeOptions(color=config$zone_color,
              fillColor=config$zone_color,fillOpacity=.08,weight=2)),
          editOptions = leaflet.extras::editToolbarOptions(),
          singleFeature = TRUE) |>
        leaflet::addLayersControl(baseGroups = beta_base_groups(),
          overlayGroups = c("Flujo", "Subidas", "Bajadas", config$zone_group),
          options = leaflet::layersControlOptions(collapsed = FALSE))
    })
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
        leaflet::clearGroup("Flujo") |> leaflet::clearGroup(config$zone_group) |>
        leaflet::removeControl("legend-endpoint")
      if (nrow(r$flow)) {
        p <- r$flow
        top <- max(p$personas)
        pal <- leaflet::colorNumeric(c("#FFFFFF", "#FF0000"), domain = c(0, top))
        opacity <- .1 + .8 * p$personas / top
        radius <- beta_flow_radius(p$personas)
        proxy <- proxy |> leaflet::addCircleMarkers(lng = p$lon, lat = p$lat,
          radius = radius, stroke = TRUE, weight = .35, color = "#7f0000",
          opacity = .8, fillColor = pal(p$personas), fillOpacity = opacity, group = "Flujo",
          popup = paste0(htmltools::htmlEscape(p$NOM_CALLE),
            "<br>ID punto: ", htmltools::htmlEscape(p$ID_punto),
            "<br>Personas: ", format(p$personas, big.mark = ".", decimal.mark = ","))) |>
          leaflet::addLegend(position = "bottomright", pal = pal, values = c(0, top), title = "Flujo seleccionado",
            group = "Flujo", layerId = "legend-endpoint")
      }
      if (!identical(r$zone_type,"poligono")) {
        shape <- NULL
        if (r$zone_type == "barrio") shape <- data$shapes$barrios[data$shapes$barrios$NOMBBARR %in% r$zones, ]
        if (r$zone_type == "municipio") shape <- data$shapes$municipio[data$shapes$municipio$MUNICIPIO %in% r$zones, ]
        if (r$zone_type == "ccz") shape <- data$shapes$ccz[data$shapes$ccz$CCZ %in% r$zones, ]
        if (r$zone_type == "departamento") shape <- data$shapes$departamento[data$shapes$departamento$DEPARTAMENTO %in% r$zones, ]
        if (!is.null(shape) && nrow(shape)) proxy |> leaflet::addPolygons(data = shape, color = config$zone_color,
          weight = 2, fillOpacity = .08, group = config$zone_group)
      }
    })
    shiny::observeEvent(input$map_groups,{
      groups <- if(is.null(input$map_groups)) character() else input$map_groups
      proxy <- leaflet::leafletProxy("map",session=session)
      if(config$zone_group %in% groups) {
        leaflet::showGroup(proxy,"Dibujo de zona")
      } else {
        leaflet::hideGroup(proxy,"Dibujo de zona")
      }
    },ignoreInit=TRUE)
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
      people <- sum(r$trips$cantidad_pasajeros)
      bslib::layout_columns(
        bslib::value_box(config$value_label, format(people, big.mark = ".", decimal.mark = ","), theme = "primary", height = "82px", fill = FALSE, class = "compact-value-box"),
        bslib::value_box("Registros OD", format(nrow(r$trips), big.mark = ".", decimal.mark = ","), theme = "light", height = "82px", fill = FALSE, class = "compact-value-box"),
        bslib::value_box("Recorridos", format(data.table::uniqueN(r$trips$recorrido), big.mark = ".", decimal.mark = ","), theme = "light", height = "82px", fill = FALSE, class = "compact-value-box"))
    })
    output$caption <- shiny::renderText({
      r <- applied()
      sprintf("%s · %s · %s entre %02d:%02d y %02d:%02d", config$caption,
        config$scope_caption[[r$other_scope]], config$time_label,
        floor(r$hours[1]), round(r$hours[1] %% 1 * 60), floor(r$hours[2]), round(r$hours[2] %% 1 * 60))
    })
    output$status <- shiny::renderText({
      r <- applied()
      sprintf("%s paradas seleccionadas · consulta %.2f s", format(length(r$stop_ids), big.mark = ".", decimal.mark = ","), r$seconds)
    })
    rank <- function(x) head(x[order(-personas), .(Parada = COD_UBIC_P, Calle = CALLE, Personas = personas)], 10)
    output$origins <- shiny::renderTable(rank(applied()$origin), striped = TRUE)
    output$destinations <- shiny::renderTable(rank(applied()$destination), striped = TRUE)
  })
}

destinationServer <- function(id, data) endpointServer(id, data, "destination")
originServer <- function(id, data) endpointServer(id, data, "origin")

