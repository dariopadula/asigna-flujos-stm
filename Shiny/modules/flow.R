flowUI <- function(id,data) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar=bslib::sidebar(width=270,
      shiny::tags$div(class="eyebrow","EXPLORAR LA RED STM"),
      shiny::h4("Tu consulta"),
      shiny::selectInput(ns("date"),"Fecha de paso",choices=as.character(data$dates),selected=as.character(data$dates[1])),
      shiny::sliderInput(ns("hours"),"Franja horaria",min=0,max=24,value=c(7,10),step=.25,post=" h"),
      shiny::radioButtons(ns("service_type"),"Tipo de servicio",
        choices=c("Ambos"="todos","Urbano"="urbano","Interdepartamental"="inter"),
        selected="todos"),
      shiny::selectizeInput(ns("lines"),"Líneas",choices=stats::setNames(data$lines$COD_LINEA,paste(data$lines$DESC_LINEA,"·",data$lines$COD_LINEA)),multiple=TRUE,options=list(placeholder="Todas las líneas")),
      shiny::radioButtons(ns("metric"),"Visualizar",choices=c("Personas a bordo"="personas","Pasadas de buses"="buses")),
      bslib::accordion(
        bslib::accordion_panel("Paso por calles (opcional)",
          shiny::selectizeInput(ns("streets"),"Calles o avenidas",choices=NULL,multiple=TRUE,
            options=list(placeholder="Sin filtro de calles",plugins=list("remove_button"))),
          shiny::selectizeInput(ns("directions"),"Sentidos",multiple=TRUE,
            choices=c("Norte → Sur"="N_S","Sur → Norte"="S_N","Este → Oeste"="E_O","Oeste → Este"="O_E"),
            options=list(placeholder="Todos los sentidos",plugins=list("remove_button"))),
          shiny::radioButtons(ns("street_display"),"Qué mostrar",
            choices=c("Sólo las calles seleccionadas"="street_only","Recorridos completos asociados"="associated")),
          shiny::conditionalPanel(sprintf("input['%s'] == 'associated'",ns("street_display")),
            shiny::radioButtons(ns("street_match"),"Si hay varias calles",inline=TRUE,
              choices=c("Alguna"="any","Todas"="all"))),
          shiny::helpText("El horario se aplica al momento en que el bus pasa por la calle."))),
      shiny::actionButton(ns("apply"),"Actualizar mapa",class="btn-primary w-100"),
      shiny::hr(),shiny::helpText("Seleccioná filtros y actualizá el mapa. El horario incluye el inicio y excluye el final."),
      shiny::helpText("El tipo de servicio se aplica a buses, personas y actividad de paradas."),
      shiny::helpText("Las capas Flujos y Paradas se encienden desde el control del mapa, sin recalcular. La categoría y cobertura de las paradas se eligen en Concentración de paradas."),
      shiny::helpText("Personas: suma de ocupación en cada punto. No es un conteo de personas únicas. Buses: incluye pasadas sin pasajeros."),
      shiny::helpText("Recorridos completos asociados selecciona personas que estaban a bordo al pasar por la calle; para buses selecciona los servicios que pasaron."),
      shiny::helpText("Con calles seleccionadas, Subidas y Bajadas usan esas personas; Pasadas de buses usa los servicios que cumplieron el filtro.")),
    bslib::navset_card_tab(
      bslib::nav_panel("Mapa",
        shiny::uiOutput(ns("summary")),
        bslib::card(bslib::card_header(shiny::textOutput(ns("caption"))),
          leaflet::leafletOutput(ns("map"),height="62vh"),
          shiny::textOutput(ns("status"))),
        bslib::card(bslib::card_header("Puntos con mayor flujo"),shiny::tableOutput(ns("ranking")),
          shiny::downloadButton(ns("download"),"Descargar puntos de la consulta"),
          shiny::downloadButton(ns("download_stops"),"Descargar actividad de paradas"))),
      bslib::nav_panel("Concentración de paradas",
        stopConcentrationUI(ns("stop_concentration"),
          c(subidas="Subidas",bajadas="Bajadas",buses="Pasadas de buses")))
    ))
}

flowServer <- function(id,data) {
  shiny::moduleServer(id,function(input,output,session) {
    street_options <- stats::setNames(data$street_choices$street_key,data$street_choices$street_label)
    shiny::updateSelectizeInput(
      session,"streets",choices=street_options,selected=character(),server=TRUE
    )
    applied <- shiny::eventReactive(input$apply,{
      shiny::req(input$date,input$hours,input$metric,input$service_type)
      shiny::validate(shiny::need(input$hours[2]>input$hours[1],"Elegí una franja de duración positiva."))
      t <- proc.time()[[3]]
      query_data <- get_beta_day_data(data,input$date)
      street_display <- if(is.null(input$street_display)) "street_only" else input$street_display
      street_match <- if(is.null(input$street_match)) "any" else input$street_match
      if(length(input$streets)) {
        result <- query_street_flow(
          query_data, input$date, input$hours,
          lines=input$lines, metric=input$metric,
          streets=input$streets, directions=input$directions,
          display_mode=street_display, match_mode=street_match,
          service_type=input$service_type
        )
      } else {
        result <- query_beta_flow(
          query_data, input$date, input$hours,
          lines=input$lines, metric=input$metric,
          service_type=input$service_type
        )
        result$street_points <- data.table::data.table()
        result$trips <- data.table::data.table()
        result$eligible_runs <- character()
        result$personas <- 0
      }
      result$stops <- if(length(input$streets)) {
        query_street_stop_activity(query_data,result)
      } else {
        query_stop_activity(
          query_data, input$date, input$hours,
          lines=input$lines, service_type=input$service_type
        )
      }
      result$seconds <- proc.time()[[3]]-t
      result$metric <- input$metric
      selected_street_labels <- data$street_choices[
        match(input$streets,street_key),street_label
      ]
      scope <- if(!length(input$streets)) {
        if(input$metric=="personas") "Pasadas de personas por punto" else "Pasadas de buses por punto"
      } else if(street_display=="street_only") {
        paste("Flujo sobre",paste(selected_street_labels,collapse=", "))
      } else {
        paste("Recorridos completos asociados a",paste(selected_street_labels,collapse=", "))
      }
      result$street_filter <- length(input$streets)>0
      service_label <- c(
        todos="Ambos servicios", urbano="Urbano",
        inter="Interdepartamental"
      )[[input$service_type]]
      result$caption <- sprintf("%s · %s · %s · %02d:%02d a %02d:%02d",scope,service_label,input$date,
        floor(input$hours[1]),round(input$hours[1]%%1*60),floor(input$hours[2]),round(input$hours[2]%%1*60))
      result
    },ignoreNULL=FALSE)
    concentration <- stopConcentrationServer("stop_concentration",shiny::reactive(applied()$stops),
      c(subidas="Subidas",bajadas="Bajadas",buses="Pasadas de buses"))
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options=leaflet::leafletOptions(preferCanvas=TRUE)) |>
        add_beta_base_tiles() |>
        leaflet::setView(lng=-56.17,lat=-34.87,zoom=12) |>
        leaflet::addLayersControl(baseGroups=beta_base_groups(),
          overlayGroups=c("Flujos","Paradas","Calles seleccionadas"),
          options=leaflet::layersControlOptions(collapsed=FALSE)) |>
        leaflet::hideGroup("Paradas")
    })
    shiny::observeEvent(applied(),{
      r <- applied(); p <- r$points
      # El flujo sólo cambia al aplicar filtros; el umbral de paradas no lo recalcula.
      proxy <- leaflet::leafletProxy("map",session=session) |>
        leaflet::clearGroup("Flujos") |> leaflet::clearGroup("Calles seleccionadas") |>
        leaflet::removeControl("legend-flow")
      if(nrow(r$street_points)) {
        proxy <- proxy |> leaflet::addCircleMarkers(lng=r$street_points$lon,lat=r$street_points$lat,
          radius=5,stroke=TRUE,weight=1.5,color="#ffd166",fill=FALSE,group="Calles seleccionadas",
          popup=paste0("<b>",htmltools::htmlEscape(r$street_points$NOM_CALLE),"</b>",
            "<br>ID punto: ",htmltools::htmlEscape(r$street_points$ID_punto),
            "<br>Sentido: ",r$street_points$dirAv))
      }
      if(nrow(p)) {
        pal <- leaflet::colorNumeric(c("#FFFFFF","#FF0000"),domain=c(0,max(p$valor)))
        opacity <- 0.1+0.8*p$valor/max(p$valor)
        labels <- paste0(htmltools::htmlEscape(p$NOM_CALLE)," · ",p$dirAv,
          "<br>ID punto: ",htmltools::htmlEscape(p$ID_punto),
          "<br>",format(p$valor,big.mark=".",decimal.mark=",",scientific=FALSE),
          if(r$metric=="personas") " pasadas de personas" else " pasadas de buses")
        radius <- beta_flow_radius(p$valor)
        proxy |> leaflet::addCircleMarkers(lng=p$lon,lat=p$lat,radius=radius,
          stroke=TRUE,weight=.35,color="#7f0000",opacity=.8,
          fillOpacity=opacity,fillColor=pal(p$valor),popup=labels,group="Flujos") |>
          leaflet::addLegend(position="bottomright",pal=pal,values=c(0,max(p$valor)),group="Flujos",layerId="legend-flow",title=if(r$metric=="personas") "Personas por punto" else "Buses por punto")
      }
    })
    shiny::observeEvent(concentration$current(),{
      all_stops <- concentration$current(); s <- all_stops[incluida == TRUE]
      metric <- concentration$metric()
      proxy <- leaflet::leafletProxy("map",session=session) |>
        leaflet::clearGroup("Paradas") |> leaflet::removeControl("legend-stops")
      if(nrow(s)) {
        value <- s[[metric]]
        visual <- beta_stop_visual(all_stops$valor)
        selected_scaled <- pmin(pmax(value / visual$top, 0), 1)
        pal_stop <- beta_stop_palette(visual$top)
        fmt <- function(x) format(x,big.mark=".",decimal.mark=",",scientific=FALSE,trim=TRUE)
        labels_stop <- paste0("<b>Parada ",s$COD_UBIC_P,"</b><br>",htmltools::htmlEscape(s$CALLE),
          "<br>Subidas: ",fmt(s$subidas),"<br>Bajadas: ",fmt(s$bajadas),"<br>Pasadas de buses: ",fmt(s$buses))
        proxy |> leaflet::addCircleMarkers(lng=s$lon,lat=s$lat,
          radius=4+7*sqrt(selected_scaled),stroke=TRUE,weight=.7,
          color="#17324d",opacity=.9,fillColor=pal_stop(value),
          fillOpacity=.55+.35*sqrt(selected_scaled),popup=labels_stop,group="Paradas") |>
          leaflet::addLegend(position="bottomleft",pal=pal_stop,
            values=c(0,visual$top),group="Paradas",layerId="legend-stops",
            title=beta_stop_legend_title(metric,visual$top))
      }
    })
    output$caption <- shiny::renderText(applied()$caption)
    output$summary <- shiny::renderUI({
      r <- applied()
      bslib::layout_columns(
        bslib::value_box("Recorridos en la franja",format(r$recorridos,big.mark=".",decimal.mark=","),theme="primary",height="82px",fill=FALSE,class="compact-value-box"),
        bslib::value_box("Puntos con flujo",format(r$puntos,big.mark=".",decimal.mark=","),theme="light",height="82px",fill=FALSE,class="compact-value-box"),
        bslib::value_box("Máximo en un punto",format(r$maximo,big.mark=".",decimal.mark=","),theme="light",height="82px",fill=FALSE,class="compact-value-box"))
    })
    output$status <- shiny::renderText({
      r <- applied(); c <- concentration$current()
      visible <- sum(c$incluida)
      street_text <- if(r$street_filter) sprintf(" %s puntos corresponden a las calles elegidas.",format(nrow(r$street_points),big.mark=".",decimal.mark=",")) else ""
      sprintf("%s Consulta: %.2f s. %s pasadas de bus con horario extrapolado o de respaldo.%s %s de %s paradas visibles.",
        if(r$puntos==0) "Sin resultados para estos filtros." else "",r$seconds,
        format(r$extrapoladas,big.mark=".",decimal.mark=","),street_text,format(visible,big.mark=".",decimal.mark=","),
        format(nrow(c),big.mark=".",decimal.mark=","))
    })
    output$ranking <- shiny::renderTable({p <- applied()$points;head(p[order(-valor),.(Calle=NOM_CALLE,Sentido=dirAv,Punto=ID_punto,Personas=personas,Buses=buses)],10)},striped=TRUE)
    output$download <- shiny::downloadHandler(filename=function() "flujos_puntos.csv",content=function(file){data.table::fwrite(applied()$points,file)})
    output$download_stops <- shiny::downloadHandler(filename=function() "actividad_paradas.csv",content=function(file){data.table::fwrite(applied()$stops,file)})
  })
}

