# Pipeline de referencias

Este pipeline construye y mantiene las referencias geográficas. No lee la OD
y no se ejecuta por cada fecha operativa.

Hay dos rutas complementarias:

1. **Ruta rápida validada:** prepara el universo de variantes y migra el
   checkpoint canónico existente conservando exactamente sus IDs y geometrías.
2. **Reconstrucción completa:** vuelve a producir la base urbana, la red OSM
   exterior y la red híbrida desde sus fuentes declaradas.

## Ruta rápida: fotografía inicial de mayo de 2026

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/01_preparar_universo_variantes.R
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/02_migrar_red_canonica_inicial.R
```

El primer script reconstruye desde Hive el universo completo al corte
20/05/2026. El segundo migra bit a bit los corredores y puntos canónicos
validados, verifica su contrato y registra huellas de origen y destino.

No se migran en este bloque las secuencias canónicas terminadas de variantes.

Salida del checkpoint:

`data/registry/canonica/red=canonica_202605_v1/`

Esta es la red que debe usarse mientras no se haya ejecutado y comparado una
reconstrucción integral nueva.

## Reconstrucción completa desde fuentes

La configuración y las rutas de las fuentes están centralizadas en
`config/referencias_202605.yml`. La secuencia es:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/03_preparar_fuentes_constructor.R
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/04_validar_fuentes_constructor.R
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/05_construir_base_urbana.R
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/06_preparar_red_osm.R
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/07_construir_red_hibrida.R
```

Cada script debe ejecutarse desde la raíz de `proyecto_flujo`. Sus funciones
reutilizables están en `pipeline_referencias/R/`.

| Etapa | Función principal | Resultado |
| --- | --- | --- |
| Preparar fuentes | `R/fuentes/preparar_fuentes_constructor.R` | Copias locales y manifiesto MD5 |
| Validar fuentes | mismo módulo | Contratos de vías, paradas, límite y OSM |
| Base urbana | `R/canonica/construir_base_urbana.R` | Calles y puntos canónicos urbanos |
| Red OSM exterior | `R/canonica/preparar_red_osm.R` | Vías OSM próximas a variantes exteriores |
| Red híbrida | `R/canonica/construir_red_hibrida.R` | Catálogos nuevos de corredores y puntos |

Las salidas de reconstrucción se escriben en
`data/intermediate/referencias/reconstruccion_canonica_202605/`; no
sobrescriben el checkpoint validado.

## Nivel de validación actual

- Checkpoint real: validado bit a bit, con 4.189 corredores y 63.647 puntos.
- Constructor completo: migrado y cubierto por pruebas sintéticas.
- Constructor de variantes: ejecutado y promovido con 3.218 variantes,
  1.069.719 posiciones, 221.336 paradas y 10 controles críticos aprobados.
- Suite actualizada después de la partición: 233 pruebas aprobadas por el
  usuario.
- Reconstrucción integral real: deliberadamente no ejecutada todavía por su
  costo. Antes de promover su resultado deberá compararse con el checkpoint.

## Variantes canónicas

La construcción está separada en checkpoints reanudables. El antiguo
`08_construir_variantes_canonicas.R` se conserva documentado en
`scripts/en_desuso/` y no forma parte del flujo vigente.

Ejecutar, siempre desde la raíz de `proyecto_flujo` y de a un paso por vez:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/08_proyectar_variantes_canonicas.R
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/09_asignar_paradas_canonicas.R
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/10_validar_promover_variantes_canonicas.R
```

El paso 08 persiste la proyección costosa; el 09 la reutiliza para asignar las
221.336 paradas; el 10 compara las claves contra el oráculo y sólo promueve si
todos los controles críticos aprueban. Los candidatos están en
`data/intermediate/referencias/variantes_canonicas/` y el registro final en
`data/registry/variantes_canonicas/`. Véase
`cerebro_flujo_stm/34_variantes_canonicas_reanudables.md`.

La ejecución inicial quedó promovida el 16/10/2026. Sus 1.069.719 claves de
secuencia coinciden exactamente con el oráculo y no presenta claves faltantes
ni adicionales.

## Registro regional de paradas

La regionalización se construye una sola vez después de promover las variantes
canónicas:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/11_construir_registro_regiones.R
```

El registro queda en
`data/registry/regiones/version=regiones_202605_v1/`. Relaciona las 13.250
paradas físicas con barrio, CCZ, municipio y departamento, conserva las formas
geográficas compartidas y valida 12 controles. Las capas del proyecto original
son fuentes de sólo lectura; las publicaciones diarias consumen exclusivamente
el registro versionado resultante.

## Registro de presentación de calles y sentidos

Este registro es estático para una versión de red y de variantes canónicas. No
lee la OD ni se recalcula por fecha operativa. Conserva la convención cardinal
validada del contrato híbrido, separa la identidad canónica del punto de su
identidad de presentación y aplica el desplazamiento cartográfico de 10 metros.

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/12_construir_registro_presentacion.R
```

La salida inmutable queda en
`data/registry/presentacion/version=presentacion_202605_v1/`. Este paso debe
validarse antes de publicar la referencia para consumo de Shiny.

La ejecución inicial produjo 1.069.719 posiciones, 61.273 identidades de
presentación y aprobó 13/13 controles. Una vez validada, se publica sin tocar
ninguna fecha diaria mediante:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" pipeline_referencias/scripts/13_publicar_registro_presentacion.R
```

