# Pipeline diario

Este directorio contendrá la orquestación y las funciones del procesamiento
diario. La migración se realizará por etapas, comparando cada resultado con el
flujo vigente antes de modificar su comportamiento.

```text
pipeline_diario/
├── R/
│   ├── ingesta/
│   ├── variantes/
│   ├── canonica/
│   ├── horarios/
│   ├── ocupacion/
│   ├── publicacion/
│   └── validacion/
├── scripts/
└── docs/
```

## Primer bloque migrado: ingesta OD diaria

La ingesta abre el CSV mensual una sola vez por operación, filtra una fecha o
un mes sin cargar la tabla completa en memoria y crea particiones diarias
persistentes con esquema, inventario, resumen y controles.

Desde `proyecto_flujo/`:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/01_ingerir_od.R 2026-05-20
# Mes completo (omite días sin filas):
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/01_ingerir_od.R 2026-05
```

La salida queda en `data/ingested/od/fecha=2026-05-20/`. Puede contener varias
partes `part-*.parquet`. El proceso no sobrescribe una fecha existente; la
opción explícita `--sobrescribir` reconstruye sólo la fecha solicitada.

Para subir una partición preparada al `ingested_root` S3 del perfil Cloudera:

```bash
Rscript pipeline_diario/scripts/01b_subir_od_ingestada.R 2026-05-20 \
  data/ingested/od/fecha=2026-05-20
```

La subida rechaza claves existentes y verifica cada objeto. El paso 02 usa la
misma estructura lógica: en local valida la partición directamente; en S3
descarga únicamente esa fecha a `execution.cache_root`.

## Segundo bloque migrado: OD operativa

Este bloque reutiliza la partición diaria, conserva `global = TRUE` y agrega la
clasificación mensual de `lineas_metro.csv`:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/02_preparar_od_operativa.R 2026-05-20
```

La salida queda en `data/intermediate/od_operativa/fecha=2026-05-20/`. Se
detiene si encuentra una línea sin catálogo o si la salida ya existe.
En Cloudera, las referencias y registros persistentes requeridos por los pasos
posteriores se sincronizan al caché local de forma explícita en los lanzadores.

## Tercer bloque: diagnóstico de variantes

El diagnóstico cruza las variantes de subida y bajada de la OD operativa con
geometrías vigentes, secuencias de paradas y líneas del mes:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/03_diagnosticar_variantes.R 2026-05-20
```

La salida queda en `data/intermediate/diagnostico_variantes/fecha=2026-05-20/`.
Este bloque sólo informa problemas; no reconstruye variantes ni modifica la
base canónica.

## Cuarto bloque: normalización de paradas

Normaliza las paradas de las variantes diagnosticadas sin alterar su orden
semántico ni proyectarlas todavía sobre la geometría:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/04_normalizar_paradas_variantes.R 2026-05-20
```

La salida queda en
`data/intermediate/paradas_variantes_normalizadas/fecha=2026-05-20/`.

## Comparación exploratoria dentro de la misma línea

Para variantes sin WKT se comparan paradas, orden y distancia contra otras
geometrías vigentes de la misma línea:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/05_comparar_variantes_misma_linea.R 2026-05-20
```

Las candidatas son semillas de revisión; nunca reemplazos automáticos.

## Quinto bloque operativo: catálogo de variantes

Une las variantes usadas por la OD con su última geometría vigente y las
paradas normalizadas. Las variantes sin geometría quedan registradas como
pendientes y no se reconstruyen automáticamente:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/06_preparar_catalogo_variantes.R 2026-05-20
```

La salida queda en
`data/intermediate/catalogo_variantes_operativas/fecha=2026-05-20/`. Este
bloque todavía no proyecta sobre la base canónica ni genera insumos Shiny.

Este catálogo completo se conserva como checkpoint de migración. No es el
modelo previsto para repetir todos sus datos cada día.

## Bloque histórico 07: inventario incremental

El inventario append-only nacido desde la OD fue retirado del flujo vigente.
Su script, función y prueba se conservan bajo carpetas `en_desuso/`, con sus
README de trazabilidad. No debe ejecutarse para nuevas fechas.

Los datos existentes en
`data/registry/variantes/lotes/fecha_deteccion=2026-05-20/` se mantienen como
evidencia de migración, pero no constituyen la fuente maestra. El flujo diario
vigente consulta el registro canónico promovido mediante el paso 08.

## Relación diaria con el registro canónico

Construye una tabla pequeña con las variantes observadas en subida o bajada y
las relaciona con su `id_version_variante` promovido:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/08_resolver_variantes_dia.R 2026-05-20
```

La salida queda en `data/intermediate/uso_variantes/fecha=AAAA-MM-DD/`. No
incluye geometrías, secuencias ni paradas. Las variantes ausentes del registro
quedan explícitamente en `variantes_no_resueltas.csv`.

## Resolución diaria de paradas canónicas

Relaciona cada `trip_id` con los índices y puntos canónicos de sus paradas de
subida y bajada. Usa la variante de subida como secuencia del viaje y deja en
cuarentena las variantes subida/bajada distintas:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/09_resolver_paradas_canonicas_dia.R 2026-05-20
```

La salida queda en
`data/intermediate/od_paradas_canonicas/fecha=AAAA-MM-DD/`. Es una relación
liviana por `trip_id`; no copia geometrías ni secuencias completas.

## Recuperación entre variantes distintas

Evalúa solamente la cuarentena del paso 09 donde las variantes de subida y
bajada difieren. Dentro de la misma línea, conserva una parada de destino
exacta sobre la variante de subida o busca una parada posterior a no más de
500 metros:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/10_recuperar_paradas_variantes_distintas.R 2026-05-20
```

La salida queda en
`data/intermediate/recuperacion_paradas/fecha=AAAA-MM-DD/`. Es un delta
compacto y auditable; no duplica la OD y todavía no es una tabla publicada
para Shiny. Las asignaciones por cercanía quedan marcadas para revisión.

## Recorridos operativos

Agrupa los viajes por fecha, empresa y evento real de subida, aplica el delta
del paso 10 y exige una única variante canónica por pasada:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/11_construir_recorridos_operativos.R 2026-05-20
```

La salida queda en
`data/intermediate/recorridos_operativos/fecha=AAAA-MM-DD/`. Contiene una fila
por recorrido procesable y una cuarentena agregada. No crea otra tabla masiva
de viajes y todavía no estima horarios.

## Horarios por recorrido

Reconstruye en memoria los viajes utilizables de los pasos 09 y 10, toma como
universo los recorridos autorizados por el paso 11 y estima la hora del bus en
cada punto canónico:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/12_estimar_horarios_recorridos.R 2026-05-20
```

La salida queda en
`data/intermediate/horarios_recorridos/fecha=AAAA-MM-DD/`. El dataset pesado
`pasadas_bus/` se escribe en partes reanudables; la calidad conserva una fila
por recorrido. No se guarda otra copia completa de los viajes y todavía no se
calcula ocupación ni se publica para Shiny.

## Ocupación por recorrido

Reconstruye en memoria los viajes utilizables, conserva únicamente los
recorridos aprobados por el paso 12, separa recorridos mayores a 36 horas y
viajes con tiempos finitos invertidos, y calcula subidas, bajadas y personas a
bordo sobre el intervalo `[origen, destino)`:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/13_calcular_ocupacion_recorridos.R 2026-05-20
```

La salida queda en
`data/intermediate/ocupacion_recorridos/fecha=AAAA-MM-DD/`. El dataset pesado
`pasadas_ocupacion/` conserva las particiones del paso 12 y se escribe mediante
un checkpoint reanudable. También se generan calidad por recorrido,
cuarentenas compactas de duración y tiempos invertidos, resumen, 17 controles
y manifiesto. No se persiste otra copia completa de los viajes ni se publica
para Shiny.

## Actividad de paradas y viajes de consulta

El paso 14 consume la ocupación promovida, vincula cada posición canónica con
sus paradas físicas y reconstruye únicamente los viajes que participaron en la
ocupación. Conserva los tiempos observados y agrega los tiempos estimados en
origen y destino:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/14_preparar_insumos_consulta_dia.R 2026-05-20
```

La salida queda en
`data/intermediate/insumos_consulta/fecha=AAAA-MM-DD/`, con
`actividad_paradas/`, `viajes_consulta/` y `calidad_insumos/` en partes
reanudables, además de resúmenes, 19 controles y manifiesto. Este paso no
regionaliza ni publica todavía para Shiny.

La corrida real del 20/05/2026 fue promovida y aprobó los 19 controles:
1.450.423 filas de actividad para 20.795 recorridos y 12.435 paradas, más
1.155.132 viajes de consulta y 1.180.454 personas. La comparación con el
oráculo confirmó igualdad exacta del componente inter y explicó la diferencia
urbana por recuperaciones del paso 10 y recorridos en cuarentena temporal.

## Publicación diaria

El paso 15 consume ocupación e insumos de consulta ya promovidos. Copia la
ocupación sin alterarla, regionaliza actividad y extremos de viaje, valida 23
controles y sólo después crea la entrada `listo` del catálogo:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_diario/scripts/15_publicar_dia.R 2026-05-20
```

La salida queda en `data/published/dias/fecha=AAAA-MM-DD/`. Las referencias
estáticas se comparten bajo `data/published/referencias/` y el catálogo usa un
YAML append-only por fecha bajo `data/published/catalogo/`. Antes de este paso
debe existir el registro regional documentado en el README del pipeline de
referencias.

Con backend S3, el paso genera y valida todo en un directorio local temporal,
sube y verifica referencias y archivos diarios, y sólo entonces sube
`catalogo/fecha=AAAA-MM-DD.yml`. No depende de renombrados atómicos remotos y
una fecha sin ese catálogo nunca queda visible para Shiny.

