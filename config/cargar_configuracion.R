fusionar_listas_config <- function(base, ambiente) {
  resultado <- base
  for (nombre in names(ambiente)) {
    valor <- ambiente[[nombre]]
    if (is.list(valor) && is.list(resultado[[nombre]])) {
      resultado[[nombre]] <- fusionar_listas_config(resultado[[nombre]], valor)
    } else {
      resultado[[nombre]] <- valor
    }
  }
  resultado
}

expandir_variables_ambiente <- function(valor) {
  if (is.list(valor)) {
    return(lapply(valor, expandir_variables_ambiente))
  }
  if (!is.character(valor)) return(valor)

  vapply(valor, function(elemento) {
    if (!grepl("^\\$\\{[A-Z][A-Z0-9_]*\\}$", elemento)) return(elemento)
    variable <- substring(elemento, 3L, nchar(elemento) - 1L)
    resuelto <- Sys.getenv(variable, unset = "")
    if (!nzchar(resuelto)) {
      stop(sprintf("Falta definir la variable de ambiente %s.", variable),
        call. = FALSE)
    }
    resuelto
  }, character(1L), USE.NAMES = FALSE)
}

es_uri_objeto <- function(ruta) {
  is.character(ruta) && length(ruta) == 1L && grepl("^s3[a-z0-9+.-]*://", ruta)
}

es_ruta_absoluta <- function(ruta) {
  grepl("^([A-Za-z]:[/\\\\]|/)", ruta)
}

unir_ubicacion <- function(raiz, ...) {
  partes <- unlist(list(...), use.names = FALSE)
  partes <- partes[nzchar(partes)]
  if (es_uri_objeto(raiz)) {
    return(paste(c(sub("/+$", "", raiz), gsub("^/+|/+$", "", partes)),
      collapse = "/"))
  }
  file.path(raiz, partes)
}

resolver_ubicacion_configurada <- function(ruta, raiz_proyecto) {
  if (es_uri_objeto(ruta)) return(sub("/+$", "", ruta))
  if (!es_ruta_absoluta(ruta)) ruta <- file.path(raiz_proyecto, ruta)
  normalizePath(ruta, winslash = "/", mustWork = FALSE)
}

resolver_fuente_od <- function(configuracion, fecha) {
  fecha <- as.Date(fecha)
  if (is.na(fecha)) stop("La fecha OD debe tener formato AAAA-MM-DD.", call. = FALSE)
  relativa <- gsub(
    "{yyyymm}", format(fecha, "%Y%m"),
    configuracion$sources$od$path_template,
    fixed = TRUE
  )
  unir_ubicacion(configuracion$storage$source_root, relativa)
}

validar_configuracion_ambiente <- function(configuracion) {
  raices_persistentes <- c(
    "source_root", "reference_root", "registry_root", "published_root",
    "quarantine_root"
  )
  faltantes <- raices_persistentes[!vapply(
    raices_persistentes,
    function(nombre) nzchar(configuracion$storage[[nombre]] %||% ""),
    logical(1L)
  )]
  if (length(faltantes)) {
    stop(sprintf("Faltan raices persistentes: %s.", paste(faltantes, collapse = ", ")),
      call. = FALSE)
  }
  if (!nzchar(configuracion$execution$work_root %||% "") ||
      !nzchar(configuracion$execution$cache_root %||% "")) {
    stop("Faltan las raices de trabajo temporal o cache.", call. = FALSE)
  }
  if (identical(configuracion$environment, "cloudera")) {
    no_s3 <- raices_persistentes[!vapply(
      raices_persistentes,
      function(nombre) es_uri_objeto(configuracion$storage[[nombre]]),
      logical(1L)
    )]
    if (length(no_s3)) {
      stop(sprintf("En cloudera estas raices deben ser URI S3: %s.",
        paste(no_s3, collapse = ", ")), call. = FALSE)
    }
    if (es_uri_objeto(configuracion$execution$work_root) ||
        es_uri_objeto(configuracion$execution$cache_root)) {
      stop("En cloudera trabajo y cache deben usar el filesystem del job.",
        call. = FALSE)
    }
  }
  invisible(configuracion)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

materializar_compatibilidad_config <- function(configuracion) {
  configuracion$paths <- list(
    od_monthly = unir_ubicacion(
      configuracion$storage$source_root,
      configuracion$sources$od$path_template
    ),
    line_catalog = unir_ubicacion(
      configuracion$storage$reference_root, configuracion$sources$line_catalog
    ),
    variant_geometries = unir_ubicacion(
      configuracion$storage$reference_root,
      configuracion$sources$variant_geometries
    ),
    variant_stops = lapply(
      configuracion$sources$variant_stops,
      function(ruta) unir_ubicacion(configuracion$storage$reference_root, ruta)
    ),
    intermediate = configuracion$execution$work_root
  )
  configuracion$daily_variant_usage$canonical_registry_root <- unir_ubicacion(
    configuracion$storage$registry_root,
    configuracion$daily_variant_usage$canonical_registry_path
  )
  configuracion$publication$output_root <- configuracion$storage$published_root
  configuracion$publication$regional_registry_root <- unir_ubicacion(
    configuracion$storage$registry_root,
    configuracion$publication$regional_registry_path
  )
  configuracion$publication$canonical_network_root <- unir_ubicacion(
    configuracion$storage$registry_root,
    configuracion$publication$canonical_network_path
  )
  configuracion
}

cargar_configuracion <- function(raiz_proyecto, perfil = NULL) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Falta instalar el paquete R 'yaml'.", call. = FALSE)
  }
  perfil <- perfil %||% Sys.getenv("ASIGNA_FLUJOS_ENV", unset = "local")
  if (!perfil %in% c("local", "cloudera")) {
    stop(sprintf("Perfil desconocido: %s.", perfil), call. = FALSE)
  }
  archivo_base <- file.path(raiz_proyecto, "config", "base.yml")
  archivo_ambiente <- file.path(
    raiz_proyecto, "config", "environments", paste0(perfil, ".yml")
  )
  configuracion <- fusionar_listas_config(
    yaml::read_yaml(archivo_base), yaml::read_yaml(archivo_ambiente)
  )
  configuracion <- expandir_variables_ambiente(configuracion)

  if (identical(perfil, "local")) {
    for (nombre in names(configuracion$storage)) {
      if (identical(nombre, "backend")) next
      configuracion$storage[[nombre]] <- resolver_ubicacion_configurada(
        configuracion$storage[[nombre]], raiz_proyecto
      )
    }
    for (nombre in names(configuracion$execution)) {
      configuracion$execution[[nombre]] <- resolver_ubicacion_configurada(
        configuracion$execution[[nombre]], raiz_proyecto
      )
    }
  } else {
    for (nombre in names(configuracion$execution)) {
      configuracion$execution[[nombre]] <- resolver_ubicacion_configurada(
        configuracion$execution[[nombre]], raiz_proyecto
      )
    }
  }

  validar_configuracion_ambiente(configuracion)
  materializar_compatibilidad_config(configuracion)
}
