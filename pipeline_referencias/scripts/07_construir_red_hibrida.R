argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
raiz_proyecto <- normalizePath(file.path(dirname(normalizePath(archivo_script)), "..", ".."), winslash = "/")
if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
configuracion <- yaml::read_yaml(file.path(raiz_proyecto, "config", "referencias_202605.yml"))
resolver <- function(ruta) normalizePath(file.path(raiz_proyecto, ruta), winslash = "/", mustWork = FALSE)
source(file.path(raiz_proyecto, "pipeline_referencias", "R", "canonica", "construir_base_urbana.R"))
source(file.path(raiz_proyecto, "pipeline_referencias", "R", "canonica", "preparar_red_osm.R"))
source(file.path(raiz_proyecto, "pipeline_referencias", "R", "canonica", "construir_red_hibrida.R"))
constructor <- configuracion$canonical_constructor
raiz_salida <- resolver(constructor$outputs$rebuild_root)
destino <- file.path(raiz_salida, "03_red_hibrida")
if (dir.exists(destino)) stop("La red hibrida reconstruida ya existe.", call. = FALSE)
base_urbana <- file.path(raiz_salida, "01_base_urbana")
red_osm <- file.path(raiz_salida, "02_red_osm")
resultado <- construir_red_hibrida(
  readRDS(file.path(red_osm, "muestras_variantes.rds")),
  readRDS(file.path(red_osm, "vias_osm_cercanas.rds")),
  cargar_limite_montevideo(
    resolver(constructor$sources$montevideo_boundary),
    configuracion$reference_snapshot$metric_crs
  ),
  readRDS(file.path(base_urbana, "calles_simplificadas.rds")),
  readRDS(file.path(base_urbana, "puntos_calles_canonicas.rds")),
  constructor$version,
  basename(constructor$sources$osm_gpkg),
  configuracion$reference_snapshot$year_month,
  constructor$exterior$corridor_component_distance_m,
  constructor$exterior$canonical_distance_m,
  constructor$exterior$corridor_p95_max_m,
  configuracion$reference_snapshot$metric_crs
)
dir.create(destino, recursive = TRUE)
saveRDS(resultado$corredores, file.path(destino, "catalogo_corredores.rds"))
saveRDS(resultado$puntos, file.path(destino, "catalogo_puntos.rds"))
saveRDS(resultado$vias_osm_seleccionadas, file.path(destino, "vias_osm_seleccionadas.rds"))
data.table::fwrite(resultado$calidad, file.path(destino, "calidad_construccion_corredores.csv"))
cat(sprintf("Red hibrida reconstruida: %s\n", destino))
