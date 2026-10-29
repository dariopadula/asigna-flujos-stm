# Proyecto Flujo

Estructura limpia para desarrollar el pipeline diario y mantener separada la
aplicación Shiny que consume sus resultados publicados.

```text
proyecto_flujo/
├── pipeline_diario/   # Procesamiento y publicación de datos
├── pipeline_referencias/ # Universo geográfico y repositorio canónico
├── Shiny/             # Aplicación consumidora
├── config/            # Configuración por ambiente
├── tests/             # Pruebas del pipeline y sus contratos
└── data/
    ├── raw/           # Fuentes transaccionales sin modificar
    ├── ingested/      # Fuentes normalizadas persistentes (Parquet diario)
    ├── reference/     # Fuentes maestras y geográficas
    ├── intermediate/  # Derivados internos reconstruibles
    ├── registry/      # Inventarios y red canónica append-only
    ├── published/     # Única fuente de datos para Shiny
    ├── quarantine/    # Casos que requieren revisión
    └── cache/         # Cache local acotada y reconstruible
```

Los datos pesados quedan fuera de Git. Los archivos `.gitkeep` conservan la
estructura de directorios en el repositorio.

## Alcance Git para Cloudera

La raíz recomendada del repositorio es esta carpeta. El `.gitignore` incluye el
flujo completo: configuración, pipeline diario, construcción de referencias,
publicación, aplicación Shiny y pruebas. Excluye datos, cachés, secretos y
código retirado.

La política, el mapa de datos en Cloudera y los comandos de revisión previos al
primer commit están en [GIT_CLOUDERA.md](GIT_CLOUDERA.md).

## Aplicación local

Desde esta carpeta, la aplicación autocontenida se abre con:

```r
shiny::runApp("Shiny")
```

Usa el perfil `local` de `config/environments/local.yml` y consume solamente
fechas completas de `data/published/`. La configuración común y el perfil
`cloudera` están documentados en `config/README.md`. En Cloudera, Shiny
sincroniza catálogo y referencias estáticas al iniciar y descarga solamente la
fecha solicitada a `execution.cache_root`.

La conversión OD, la materialización local/S3, la publicación con catálogo al
final y el procedimiento operativo de una fecha están documentados en
`pipeline_diario/README.md` y `R/almacenamiento/GUIA_S3_CLOUDERA.md`.
El diagrama completo del recorrido de datos, con una vista local y otra para
Cloudera/S3, está en
[pipeline_diario/FLUJO_COMPLETO.md](pipeline_diario/FLUJO_COMPLETO.md).

El punto exacto de reanudación, los resultados de la prueba en Cloudera y las
decisiones todavía pendientes se mantienen en [ESTADO_ACTUAL.md](ESTADO_ACTUAL.md).

## Código en desuso

Cualquier carpeta de código `R/` o `scripts/` puede contener una subcarpeta
`en_desuso/` cuando exista código retirado que convenga conservar como
referencia. No se crean carpetas vacías: se agregan sólo cuando haya un archivo
realmente reemplazado.

Cada `en_desuso/` debe incluir un `README.md` que explique, para cada archivo o
función:

- qué responsabilidad tenía;
- por qué fue retirado;
- fecha del retiro;
- archivo o función que lo reemplaza, si existe.

El código dentro de `en_desuso/` no debe ser cargado, ejecutado ni usado por el
pipeline vigente. No se envía allí código solamente experimental o pendiente:
debe estar confirmado que quedó fuera de uso.

