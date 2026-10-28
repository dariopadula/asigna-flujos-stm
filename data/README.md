# Organización de datos

## `raw/`

Fuentes transaccionales originales, sin modificar. La matriz OD mensual
`matriz_od_202605` debe copiarse aquí conservando su formato y nombre de origen.

## `ingested/`

Fuentes persistentes ya normalizadas para el pipeline. La OD se almacena como
un dataset Parquet particionado por fecha bajo
`ingested/od/fecha=AAAA-MM-DD/`. Cada fecha puede contener una o varias partes
Parquet y su manifiesto de validacion.

Esta capa se genera a partir del CSV mensual original o se promueve desde una
fuente Parquet diaria validada. Debe conservar el mismo contrato logico en
local y S3; no es una carpeta temporal y no debe confundirse con
`intermediate/`.

## `reference/`

Fuentes maestras o geográficas necesarias para interpretar la OD. Aquí deben
ubicarse inicialmente:

- `v_uptu_lsv_hive.csv`: geometrías y vigencias de variantes urbanas e inter;
- `v_uptu_paradas_hive0.csv` y `v_uptu_paradas_hive1.csv`: particiones de
  paradas y ordinales por variante;
- `lineas_metro.csv`: líneas, empresas y subsistemas.

El pipeline dispone de un checkpoint canónico inicial validado y del código
para reconstruir la red urbana–OSM. Las fuentes pesadas del constructor se
preparan bajo `reference/constructor_canonica/`; no se reconstruyen por fecha
OD. Véanse `cerebro_flujo_stm/32_bootstrap_referencias_canonicas_limpio.md` y
`33_auditoria_proyecto_flujo_y_hoja_de_ruta.md`.

## `intermediate/`

Resultados internos reconstruibles: OD operativa, proyecciones, secuencias,
horarios y cálculos previos a la publicación. La OD normalizada persistente
pertenece a `ingested/`, no a esta capa.

La construcción de variantes usa candidatos reanudables bajo
`intermediate/referencias/variantes_canonicas/`: primero proyección y
secuencias, luego asignación de paradas y finalmente validación.

## `published/`

Artefactos diarios validados. Esta será la única carpeta de datos consumida por
Shiny. Cada fecha vive bajo `dias/fecha=AAAA-MM-DD/`; las referencias
geográficas compartidas se publican bajo `referencias/` y una fecha sólo es
elegible cuando existe su entrada append-only en `catalogo/` con estado
`listo`.

## `registry/`

Inventarios persistentes y append-only. Las variantes, versiones geométricas y
listas normalizadas de paradas se agregan sólo cuando aparecen por primera vez.
Cada lote también guarda una relación diaria liviana que permite reproducir qué
versiones fueron seleccionadas para una fecha operativa.

El maestro inicial de secuencias se promueve bajo
`registry/variantes_canonicas/` solamente después de comparar el candidato con
el oráculo y aprobar todos los controles críticos. No debe confundirse con el
inventario diario provisional de `registry/variantes/`.

La relación estable entre paradas físicas y regiones se guarda bajo
`registry/regiones/version=.../`; se construye una vez por versión de
referencias y no por fecha OD.

## `quarantine/`

Viajes, variantes, corredores o corridas que no pueden publicarse y requieren
diagnóstico o revisión manual.

## `cache/`

Materializaciones locales reconstruibles, incluidas las cachés de Shiny y de
transferencias remotas. En Cloudera debe apuntar al disco local o efímero del
servicio, no al repositorio.

Los contenidos de estas ocho carpetas están excluidos de Git; sólo se
versiona su estructura mediante `.gitkeep`.

