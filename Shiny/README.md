# Aplicación Shiny autocontenida

La aplicación consume exclusivamente publicaciones validadas. En local usa
`data/published/`; en Cloudera usa `storage.published_root` S3 mediante un caché
local. No procesa la matriz OD ni reconstruye horarios, ocupación o referencias.

Todo su código activo vive bajo `Shiny/`:

- `app.R`: composición de la aplicación;
- `R/`: adaptador, consultas y presentación;
- `modules/`: Flujo general, Origen, Destino y Origen–destino;
- `components/`: UI y servidor principales;
- `scripts/`: validaciones reales y lanzador local.

No carga archivos de `R/` ni `Shiny/beta/` del nivel superior.

## Abrir localmente

Desde la raíz de `proyecto_flujo/`, ejecutar:

```r
source("Shiny/scripts/02_abrir_beta_publicada.R")
```

También puede abrirse directamente con:

```r
shiny::runApp("Shiny")
```

La aplicación sólo muestra fechas cuyo catálogo tiene `estado: listo` y carga
las colecciones del día al seleccionarlo.

## Ejecutar con publicaciones S3

Con `ASIGNA_FLUJOS_ENV=cloudera` y las variables de `.Renviron.example`
completas, el arranque sincroniza el catálogo, los manifiestos diarios pequeños
y las referencias estáticas compatibles. No descarga la historia diaria. Al
seleccionar una fecha, materializa únicamente `dias/fecha=AAAA-MM-DD/`, valida
el inventario y los conteos, y reutiliza esa copia durante la sesión.

Si S3 no está disponible, falta un objeto o una fecha no coincide con sus
manifiestos, la carga se detiene con un mensaje explícito y la fecha incompleta
no se expone.

## Validar una fecha publicada

Desde la raíz de `proyecto_flujo/`:

```r
rscript <- file.path(R.home("bin"), "Rscript.exe")
system2(
  rscript,
  c("Shiny/scripts/01_validar_adaptador_publicado.R", "2026-05-20")
)
system2(
  rscript,
  c("Shiny/scripts/03_validar_consultas_publicadas.R", "2026-05-20")
)
```

Las mismas validaciones aceptan `2026-05-21`. Los nombres anteriores que
terminan en `20260520.R` se mantienen como alias de compatibilidad.

## Estado validado

- 20/05/2026: 16 controles de carga y 12 controles de consultas aprobados;
- 21/05/2026: 16 controles de carga y 12 controles de consultas aprobados;
- suite completa de `proyecto_flujo/tests/testthat`: aprobada;
- arranque de `Shiny/app.R` como aplicación autocontenida: aprobado.

La aplicación usa la configuración común de `config/`. El perfil predeterminado
es `local`; el perfil `cloudera` conserva el mismo contrato de datos mediante
materialización selectiva en caché.

