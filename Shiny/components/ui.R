betaUI <- function(data) {
  bslib::page_fluid(
    theme=bslib::bs_theme(version=5,bootswatch="flatly",primary="#087f83",base_font="system-ui"),
    shiny::tags$head(shiny::tags$style(shiny::HTML("body{background:#f2f5f5}.app-header{padding:24px 8px 18px}.eyebrow{letter-spacing:.15em;font-size:.75rem;font-weight:700;color:#087f83}.app-header h1{font-weight:750;margin:6px 0}.card{border:0;box-shadow:0 2px 12px #1632380b}.beta-tag{background:#daf0eb;border-radius:20px;padding:5px 12px;font-size:.8rem}.bslib-sidebar-layout{border:0}.leaflet-container{border-radius:8px}.leaflet-control.info.legend{font-size:.72rem;line-height:1.15;padding:5px 7px;max-width:145px;white-space:normal}.leaflet-control.info.legend i{width:14px;height:14px}.leaflet-control.info.legend .legend-detail{font-size:.66rem;color:#52666b}.compact-value-box{height:82px!important;min-height:82px!important}.compact-value-box .card-body{padding:.45rem .75rem}.compact-value-box .value-box-title{font-size:.7rem;margin-bottom:.15rem}.compact-value-box .value-box-value{font-size:1.25rem;line-height:1.05}.od-sidebar{font-size:.9rem}.od-sidebar h4{font-size:1.05rem;line-height:1.2;margin-bottom:.35rem}.od-sidebar .control-label,.od-sidebar .form-label{font-size:.82rem;margin-bottom:.2rem}.od-sidebar .form-group{margin-bottom:.45rem}.od-sidebar .shiny-options-group{line-height:1.2}.od-sidebar hr{margin:.55rem 0}.od-sidebar .help-block{font-size:.76rem;line-height:1.25;margin-top:.4rem}"))),
    shiny::tags$header(class="app-header",shiny::span(class="eyebrow","STM · LABORATORIO DE MOVILIDAD"),shiny::h1("Flujos de la red STM ",shiny::span(class="beta-tag","BETA")),
      shiny::p("Explorá cómo se mueven las personas y los buses a lo largo del día.")),
    bslib::navset_card_tab(
      bslib::nav_panel("Flujo general",flowUI("flow",data)),
      bslib::nav_panel("Origen",originUI("origin",data)),
      bslib::nav_panel("Destino",destinationUI("destination",data)),
      bslib::nav_panel("Origen–destino",odUI("od",data))
    ),
    shiny::tags$footer(style="padding:20px;color:#52666b",sprintf("Datos %s · %s recorridos con horario completo · %s pendientes. Horarios estimados; beta para validación.",
      data$source_label,
      sum(data$quality$estado=="horario_completo"),sum(data$quality$estado!="horario_completo")),
      shiny::p(if (data$profile %in% c(
          "piloto_diario_regiones", "piloto_diario_departamentos",
          "publicado_limpio"
        ))
        "Cada fecha corresponde al día operativo publicado; los servicios que cruzan medianoche permanecen en ese día. El fondo cartográfico requiere conexión a Internet."
      else
        "Las fechas adyacentes incluyen extensiones del servicio. El fondo cartográfico requiere conexión a Internet.")))
}

