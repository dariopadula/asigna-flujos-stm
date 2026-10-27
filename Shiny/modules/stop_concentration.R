stopConcentrationPanel <- function(ns, metric, label) {
  shiny::tagList(
    shiny::uiOutput(ns(paste0("summary_",metric))),
    bslib::card(
      shiny::sliderInput(ns(paste0("coverage_",metric)),"Mostrar paradas hasta cubrir",
        min=50,max=100,value=80,step=5,post="%")),
    bslib::card(bslib::card_header(paste("Actividad por parada ·",label)),
      shiny::plotOutput(ns(paste0("distribution_",metric)),height="380px")),
    bslib::card(bslib::card_header(paste("Concentración acumulada ·",label)),
      shiny::plotOutput(ns(paste0("cumulative_",metric)),height="380px")),
    bslib::card(bslib::card_header(paste("Paradas con más",tolower(label))),
      shiny::tableOutput(ns(paste0("ranking_",metric))))
  )
}

stopConcentrationUI <- function(id, metrics=c(subidas="Subidas",bajadas="Bajadas")) {
  ns <- shiny::NS(id)
  panels <- lapply(names(metrics),function(metric) {
    bslib::nav_panel(unname(metrics[[metric]]),value=metric,
      stopConcentrationPanel(ns,metric,unname(metrics[[metric]])))
  })
  do.call(bslib::navset_card_tab,c(list(id=ns("metric"),selected=names(metrics)[1]),panels))
}

stopConcentrationServer <- function(id, stops, metrics=c(subidas="Subidas",bajadas="Bajadas")) {
  shiny::moduleServer(id,function(input,output,session) {
    by_metric <- list()
    for(metric_name in names(metrics)) local({
      metric <- metric_name
      metric_label <- unname(metrics[[metric]])
      metric_concentration <- shiny::reactive({
        coverage <- input[[paste0("coverage_",metric)]]
        shiny::req(coverage)
        calculate_stop_concentration(stops(),metric,coverage/100)
      })
      by_metric[[metric]] <<- metric_concentration
      output[[paste0("summary_",metric)]] <- shiny::renderUI({
        summary <- summarize_stop_concentration(metric_concentration())
        bslib::layout_columns(
          bslib::value_box("Paradas visibles",format(summary$visible,big.mark=".",decimal.mark=","),theme="primary",height="82px",fill=FALSE,class="compact-value-box"),
          bslib::value_box("Paradas activas",format(summary$active,big.mark=".",decimal.mark=","),theme="light",height="82px",fill=FALSE,class="compact-value-box"),
          bslib::value_box("Porcentaje de paradas",sprintf("%.1f%%",summary$stop_percentage),theme="light",height="82px",fill=FALSE,class="compact-value-box"),
          bslib::value_box("Actividad capturada",sprintf("%.1f%%",summary$captured_percentage),theme="light",height="82px",fill=FALSE,class="compact-value-box"),
          col_widths=c(3,3,3,3))
      })
      output[[paste0("distribution_",metric)]] <- shiny::renderPlot({
        concentration <- metric_concentration()
        shiny::validate(shiny::need(nrow(concentration)>0,"No hay actividad de paradas para estos filtros."))
        summary <- summarize_stop_concentration(concentration)
        graphics::par(mar=c(3.6,4.2,1,.6),las=1,cex.axis=.72,cex.lab=.82)
        graphics::plot(100*concentration$porcentaje_paradas,concentration$valor,type="h",lwd=1,col="#087f83",
          xlim=c(0,100),xlab="Paradas ordenadas (% del total)",ylab=metric_label)
        graphics::abline(v=summary$stop_percentage,col="#ff6b57",lty=2,lwd=2)
      },res=110)
      output[[paste0("cumulative_",metric)]] <- shiny::renderPlot({
        concentration <- metric_concentration()
        shiny::validate(shiny::need(nrow(concentration)>0,"No hay actividad de paradas para estos filtros."))
        summary <- summarize_stop_concentration(concentration)
        cutoff_x <- summary$stop_percentage
        graphics::par(mar=c(3.6,4.2,1,.6),las=1,cex.axis=.72,cex.lab=.82)
        graphics::plot(100*concentration$porcentaje_paradas,100*concentration$porcentaje_acumulado,type="l",lwd=3,col="#087f83",
          xlim=c(0,100),ylim=c(0,100),xlab="Paradas ordenadas (% del total)",ylab="Actividad acumulada (%)")
        graphics::abline(h=summary$captured_percentage,v=cutoff_x,col="#ff6b57",lty=2,lwd=2)
        graphics::points(cutoff_x,summary$captured_percentage,pch=19,col="#ff6b57",cex=.9)
        label_x <- cutoff_x+if(cutoff_x<=70) 2 else -2
        alignment <- if(cutoff_x<=70) c(0,1) else c(1,1)
        graphics::text(label_x,max(4,summary$captured_percentage-4),
          labels=sprintf("%.1f%% de paradas → %.1f%% de actividad",cutoff_x,summary$captured_percentage),
          adj=alignment,col="#8a2f25",cex=.66)
      },res=110)
      output[[paste0("ranking_",metric)]] <- shiny::renderTable({
        concentration <- metric_concentration()
        head(concentration[,.(Parada=COD_UBIC_P,Calle=CALLE,Total=valor,
          `Acumulado (%)`=round(100*porcentaje_acumulado,1))],15)
      },striped=TRUE)
    })
    list(
      metric=shiny::reactive(input$metric),
      current=shiny::reactive({
        shiny::req(input$metric)
        by_metric[[input$metric]]()
      }),
      by_metric=by_metric
    )
  })
}

