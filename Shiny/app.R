resolver_raiz_proyecto_shiny <- function() {
  candidatas <- c(".", "..")
  validas <- candidatas[file.exists(file.path(
    candidatas, "config", "base.yml"
  ))]
  if (!length(validas)) {
    stop(
      "No se encontro config/base.yml. Ejecute la app desde proyecto_flujo.",
      call. = FALSE
    )
  }
  normalizePath(validas[[1L]], winslash = "/", mustWork = TRUE)
}

if (.Platform$OS.type == "windows") {
  try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE)
}

raiz_proyecto <- resolver_raiz_proyecto_shiny()
directorio_app <- file.path(raiz_proyecto, "Shiny")
source(file.path(raiz_proyecto, "config", "cargar_configuracion.R"))
configuracion_app <- cargar_configuracion(raiz_proyecto)
cache_sass <- file.path(configuracion_app$execution$cache_root, "shiny", "sass")
dir.create(cache_sass, recursive = TRUE, showWarnings = FALSE)
options(sass.cache = cache_sass)

paquetes <- c(
  "shiny", "bslib", "leaflet", "leaflet.extras", "arrow", "sf",
  "data.table", "htmltools", "yaml"
)
faltantes <- paquetes[!vapply(
  paquetes, requireNamespace, logical(1L), quietly = TRUE
)]
if (length(faltantes)) {
  stop(sprintf("Falta instalar: %s.", paste(faltantes, collapse = ", ")),
    call. = FALSE)
}

archivos_r <- c(
  "R/consultas_temporales.R",
  "R/adaptador_datos_publicados.R",
  "R/datos_app.R",
  "R/ocupacion.R",
  "R/actividad_paradas.R",
  "R/flujos_extremos.R",
  "R/consultas_flujo.R",
  "R/presentacion_mapas.R",
  "modules/stop_concentration.R",
  "modules/flow.R",
  "modules/endpoint.R",
  "modules/od.R",
  "components/ui.R",
  "components/server.R"
)
for (archivo in archivos_r) {
  source(file.path(directorio_app, archivo), local = TRUE)
}

datos <- load_beta_data(raiz_proyecto, configuracion_app)
shiny::shinyApp(betaUI(datos), betaServer(datos))

