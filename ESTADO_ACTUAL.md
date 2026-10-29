# Estado actual — 2026-10-29

## Resultado alcanzado

El día OD `2026-05-22` completó en Cloudera el recorrido desde la ingesta S3
hasta la publicación consumible por Shiny:

```text
Parquet OD → S3 ingested → pasos 02–14 → paso 15 → S3 published → Shiny
```

- La OD diaria quedó almacenada en `ASIGNA_FLUJOS_INGESTED_ROOT`.
- `data/reference` y `data/registry` fueron provisionados en sus raíces S3.
- Los pasos 02, 03, 04, 06 y 08–15 terminaron correctamente.
- El paso 05 sigue siendo opcional y el 07 permanece retirado.
- El adaptador Shiny cargó correctamente catálogo, referencias y día desde S3.
- La suite local completa aprobó después de los cambios.

El flujo completo y la persistencia de cada conjunto están documentados en
[`pipeline_diario/FLUJO_COMPLETO.md`](pipeline_diario/FLUJO_COMPLETO.md).

## Contratos vigentes

### Entrada OD

El productor externo entrega uno o varios Parquet por día:

```text
ingested/od/fecha=AAAA-MM-DD/
└── part-*.parquet
```

No debe generar `manifiesto.yml`. El pipeline descubre todas las partes,
valida el esquema y calcula internamente filas, tamaños y MD5. El script
`01b_subir_od_ingestada.R` existe como mecanismo de carga manual y sube
únicamente los Parquet.

### Referencias y registros

Son insumos persistentes y versionados, no archivos diarios:

- `ASIGNA_FLUJOS_REFERENCE_ROOT`: líneas, variantes, paradas y fuentes
  geográficas.
- `ASIGNA_FLUJOS_REGISTRY_ROOT`: variantes canónicas, red canónica, regiones
  y presentación.

En Cloudera se materializan selectivamente bajo `execution.cache_root`. No se
guardan en Git y deben provisionarse antes de procesar la primera fecha.

### Publicación

El paso 15 construye y valida localmente en el runtime, reutiliza referencias
estáticas S3 cuando su contenido es equivalente, sube el día y escribe
`catalogo/fecha=AAAA-MM-DD.yml` al final. Shiny solo muestra fechas cuyo
catálogo tiene `estado: listo`.

## Problemas encontrados y resueltos

1. La ingesta dependía del manifiesto externo y rechazaba Parquet válidos si
   tamaño o MD5 no coincidían. Ahora el manifiesto no forma parte del contrato.
2. El paso 14 buscaba un checkpoint de referencias intermedio y local. Ahora
   usa las paradas canónicas promovidas en `registry_root`.
3. El paso 15 comparaba manifiestos estáticos byte por byte, aunque contienen
   fechas de creación variables. Ahora compara datos por contenido y
   manifiestos por campos relevantes.
4. El validador Shiny 01 leía `data/published` local incluso con perfil
   Cloudera. Ahora usa el mismo materializador S3 que la aplicación.
5. Los validadores Shiny solo admitían regresiones conocidas para el 20 y 21.
   Ahora los días nuevos se validan mediante controles estructurales y de
   consistencia, conservando regresiones exactas para las fechas documentadas.

## Próximo control

Falta ejecutar la validación funcional de consultas para el día publicado:

```r
fecha <- "2026-05-22"
system2(
  "Rscript",
  c("Shiny/scripts/03_validar_consultas_publicadas.R", fecha)
)
```

Si aprueba, el flujo queda validado de extremo a extremo, incluida la lógica
de consultas que utiliza la aplicación.

## Decisión pendiente: republicar días corregidos

La publicación aún rechaza una segunda publicación de la misma fecha. Queda
pendiente implementar republicación versionada con retención acotada:

- construir y validar una versión nueva sin alterar la activa;
- cambiar el catálogo solo cuando la versión esté completa;
- conservar la versión activa y, como máximo, una anterior para rollback;
- eliminar versiones más antiguas después de confirmar el cambio.

## Seguridad y Git

- `.Renviron` contiene valores privados, está ignorado y no debe subirse.
- No se documentan bucket, prefijos privados, tokens ni credenciales.
- Los archivos pesados de `data/` no pertenecen a Git.
- Antes de continuar en Cloudera, confirmar que los cambios locales estén
  publicados y ejecutar `git pull`.
