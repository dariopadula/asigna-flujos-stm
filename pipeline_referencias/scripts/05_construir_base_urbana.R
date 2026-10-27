argumentos_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", argumentos_completos, value = TRUE)[1])
if (is.na(archivo_script) || !nzchar(archivo_script)) stop("No se pudo determinar la ubicacion del script.", call. = FALSE)
raiz_proyecto <- normalizePath(file.path(dirname(normalizePath(archivo_script)), "..", ".."), winslash = "/")
if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
configuracion <- yaml::read_yaml(file.path(raiz_proyecto, "config", "referencias_202605.yml"))
resolver <- function(ruta) normalizePath(file.path(raiz_proyecto, ruta), winslash = "/", mustWork = FALSE)
source(file.path(raiz_proyecto, "pipeline_referencias", "R", "canonica", "construir_base_urbana.R"))
parametros <- configuracion$canonical_constructor$urban
fuentes <- configuracion$canonical_constructor$sources
destino <- file.path(resolver(configuracion$canonical_constructor$outputs$rebuild_root), "01_base_urbana")
if (dir.exists(destino)) stop("La base urbana reconstruida ya existe.", call. = FALSE)
paradas <- cargar_paradas_historicas_urbanas(
  resolver(fuentes$historical_stops), parametros$source_crs,
  configuracion$reference_snapshot$metric_crs
)
vias <- cargar_vias_urbanas(resolver(fuentes$urban_streets), configuracion$reference_snapshot$metric_crs)
resultado <- construir_base_urbana(
  vias, paradas, parametros$street_sample_distance_m,
  parametros$street_grid_cellsize_max_m, parametros$street_grid_initial_parts,
  configuracion$reference_snapshot$metric_crs
)
dir.create(destino, recursive = TRUE)
saveRDS(resultado$calles, file.path(destino, "calles_simplificadas.rds"))
saveRDS(resultado$puntos, file.path(destino, "puntos_calles_canonicas.rds"))
saveRDS(resultado$indice_variante_calle, file.path(destino, "calles_con_paradas_por_variante.rds"))
data.table::fwrite(resultado$metricas, file.path(destino, "metricas_calles_canonicas.csv"))
cat(sprintf("Base urbana reconstruida: %s\n", destino))
