# Preparación del repositorio para Cloudera

## Alcance recomendado

La raíz del repositorio debe ser esta carpeta, `proyecto_flujo/`, no la carpeta
superior `asigna-flujos-stm-rf/`. De esta forma el Git contiene solamente la
implementación limpia y no incorpora pilotos, comparaciones, resultados ni
documentación histórica del refactor.

El alcance preparado contiene **el flujo completo**: ingesta, procesamiento,
construcción de referencias, generación de insumos publicados y aplicación
Shiny. Git transporta el código; los datos viven en el almacenamiento del
ambiente.

### Se versiona

- `config/base.yml`, `config/environments/` y el cargador común;
- `config/referencias_202605.yml`, porque describe la construcción versionada
  de referencias, aunque sus orígenes de migración todavía deben sustituirse
  por artefactos provisionados en Cloudera;
- `pipeline_diario/R/` y `pipeline_diario/scripts/` activos;
- `pipeline_referencias/R/` y `pipeline_referencias/scripts/` activos;
- `Shiny/`, incluyendo aplicación, módulos, consultas y scripts de validación;
- pruebas del pipeline, publicación, referencias y aplicación;
- README, documentación operativa, `.gitignore` y `.gitattributes`;
- estructura vacía de `data/` mediante `.gitkeep`.

### No se versiona

- ningún dato raw, ingested, referencia materializada, registro, intermedio,
  publicación, cuarentena o caché;
- código retirado bajo `en_desuso/`;
- la configuración histórica `config/pipeline.yml`;
- credenciales, archivos `.env`, claves, logs, sesiones de RStudio o librerías
  instaladas;
- formatos de datos pesados aunque aparezcan accidentalmente fuera de
  `data/`.

La aplicación y el pipeline comparten configuración y contrato de publicación.
Versionarlos juntos permite desplegar ambos desde el mismo commit sin guardar
ningún Parquet, RDS, CSV operativo ni caché en Git.

## Dónde vive cada dato en Cloudera

Git transporta código y configuración, no datos. El contrato actual del perfil
`cloudera` espera:

| Variable | Contenido | Persistencia esperada |
| --- | --- | --- |
| `ASIGNA_FLUJOS_SOURCE_ROOT` | OD y fuentes de ingesta | almacenamiento persistente |
| `ASIGNA_FLUJOS_INGESTED_ROOT` | OD diaria normalizada en Parquet | almacenamiento persistente |
| `ASIGNA_FLUJOS_REFERENCE_ROOT` | fuentes geográficas/maestras | almacenamiento persistente |
| `ASIGNA_FLUJOS_REGISTRY_ROOT` | registros canónicos versionados | almacenamiento persistente |
| `ASIGNA_FLUJOS_PUBLISHED_ROOT` | insumos finales consumibles por la app | almacenamiento persistente |
| `ASIGNA_FLUJOS_QUARANTINE_ROOT` | evidencia que requiere revisión | almacenamiento persistente |
| `ASIGNA_FLUJOS_WORK_ROOT` | intermedios y checkpoints del job | disco de trabajo del job |
| `ASIGNA_FLUJOS_CACHE_ROOT` | materializaciones reconstruibles | disco local/efímero |

El perfil actual valida URI S3 para las seis raíces persistentes. Si el
ambiente final usa HDFS u otro servicio en vez de S3, se necesita implementar
ese backend; no alcanza con cambiar el texto de las variables.

La aplicación debe consumir únicamente publicaciones completas y referencias
versionadas desde `published_root`. Esos objetos no deben copiarse al Git.

## Riesgo conocido antes de ejecutar en Cloudera

`config/referencias_202605.yml` contiene una sección `migration_origins` y
algunas fuentes regionales que apuntan al proyecto histórico. Son evidencia de
migración, no rutas válidas de Cloudera. El pipeline diario puede operar con
registros ya promovidos, pero una reconstrucción completa de referencias no
será portable hasta provisionar esas fuentes en el almacenamiento del ambiente
y actualizar su configuración. Véase T03 de la hoja de ruta.

## Comandos que debe ejecutar el usuario

Desde PowerShell:

```powershell
Set-Location "RUTA\AL\proyecto_flujo"
git init -b main
git status --short --ignored
```

Antes de agregar archivos, comprobar algunos casos críticos:

```powershell
git check-ignore -v data/raw/stm_od_202605.csv
git check-ignore -v data/published/dias/fecha=2026-05-20/manifiesto.yml
git check-ignore -v data/cache/shiny/sass/01d3682ca152f478bac9e3ae30127444
git check-ignore -v pipeline_diario/scripts/en_desuso/07_actualizar_inventario_variantes.R
```

Todos los elementos anteriores deben aparecer como ignorados. En cambio,
`Shiny/app.R` debe quedar incluido:

```powershell
git check-ignore -q Shiny/app.R
if ($LASTEXITCODE -eq 0) { throw "Shiny/app.R no debe estar ignorado" }
```

Luego:

```powershell
git add .
git status --short
git diff --cached --stat
git diff --cached --name-only
```

En la lista preparada deben aparecer `Shiny/` y sus pruebas. No debe aparecer
ningún archivo bajo `en_desuso/` ni extensiones de datos como `.csv`,
`.parquet`, `.rds`, `.gpkg` o `.shp`. Sí deben aparecer los `.gitkeep` de
`data/`.

Comprobaciones adicionales antes del primer commit:

```powershell
git diff --cached --name-only | Select-String -Pattern '\.(csv|tsv|parquet|rds|fst|gpkg|shp|dbf|pem|key|p12|pfx)$'
git grep --cached -n -I -E 'AKIA[0-9A-Z]{16}|BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY'
```

Ambos comandos deben terminar sin resultados. Revisar manualmente la salida de
`git diff --cached` antes de crear el commit y configurar el remoto.

No se incluyen aquí comandos de `commit`, creación de remoto o `push`: deben
ejecutarse sólo después de revisar el contenido staged y elegir el repositorio
destino.

## Validación realizada antes de entregar

El 27/10/2026 se simuló un índice Git sin inicializar el repositorio real:

- 129 archivos candidatos;
- 640,44 KB en total;
- 1.179 archivos locales ignorados;
- 21 archivos de aplicación y 5 pruebas específicas de Shiny incluidos;
- ningún formato de datos inesperado;
- ningún candidato mayor a 1 MB;
- copia limpia construida sólo con los candidatos;
- suite completa en la copia limpia: 348 pruebas aprobadas, 0 fallos, 0
  advertencias de test y 0 omisiones.

El 28/10/2026, después de incorporar almacenamiento intercambiable,
materialización selectiva y Shiny S3, la suite del repositorio de trabajo
terminó con 397 pruebas aprobadas, 0 fallos, 0 advertencias de test y 0
omisiones.
