argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
raiz_proyecto <- normalizePath(file.path(dirname(normalizePath(archivo_script)), "..", ".."), winslash = "/")
if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
configuracion <- yaml::read_yaml(file.path(raiz_proyecto, "config", "referencias_202605.yml"))
resolver <- function(ruta) normalizePath(file.path(raiz_proyecto, ruta), winslash = "/", mustWork = FALSE)
source(file.path(raiz_proyecto, "pipeline_referencias", "R", "canonica", "preparar_red_osm.R"))
constructor <- configuracion$canonical_constructor
destino <- file.path(resolver(constructor$outputs$rebuild_root), "02_red_osm")
if (dir.exists(destino)) stop("La preparacion OSM reconstruida ya existe.", call. = FALSE)
archivo_universo <- file.path(
  resolver(configuracion$paths$intermediate_references),
  paste0("entrega=", configuracion$reference_snapshot$id), "universo_variantes.parquet"
)
muestras <- muestrear_universo_variantes(
  archivo_universo, constructor$exterior$variant_sample_distance_m,
  configuracion$reference_snapshot$metric_crs
)
limite <- cargar_limite_montevideo(
  resolver(constructor$sources$montevideo_boundary),
  configuracion$reference_snapshot$metric_crs
)
resultado <- preparar_red_osm(
  muestras, limite, resolver(constructor$sources$osm_gpkg),
  constructor$exterior$osm_buffer_m, constructor$exterior$osm_tile_m
)
dir.create(destino, recursive = TRUE)
saveRDS(resultado$muestras, file.path(destino, "muestras_variantes.rds"))
saveRDS(resultado$vias, file.path(destino, "vias_osm_cercanas.rds"))
data.table::fwrite(resultado$calidad, file.path(destino, "calidad_insumos_osm.csv"))
cat(sprintf("Red OSM preparada: %s\n", destino))
