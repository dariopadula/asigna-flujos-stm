# Guia operativa S3 desde Cloudera

Esta guia describe el procedimiento comprobado para conectar R con la conexion
`S3 Object Store` de Cloudera, subir o descargar archivos y verificar el
resultado. No se deben guardar credenciales, tokens ni valores privados en Git.

## 1. Preparar la configuracion privada

Desde la raiz de `proyecto_flujo`, crear el archivo privado a partir de la
plantilla versionada:

```bash
cp .Renviron.example .Renviron
```

Completar en `.Renviron` los valores reales:

```text
ASIGNA_FLUJOS_ENV=local
ASIGNA_FLUJOS_S3_CONNECTION="S3 Object Store"
ASIGNA_FLUJOS_S3_BUCKET=REEMPLAZAR_BUCKET
ASIGNA_FLUJOS_S3_PREFIX=REEMPLAZAR_PREFIJO/data
RETICULATE_PYTHON=/usr/local/bin/python3
```

Mientras Shiny consuma los archivos del proyecto, se debe mantener
`ASIGNA_FLUJOS_ENV=local`. Activar `cloudera` requiere habilitar previamente la
lectura S3 de la aplicacion.

La ruta de Python debe confirmarse en cada runtime:

```bash
which python3
python3 -c "import boto3; print('boto3 OK')"
```

Si se modifica `.Renviron`, reiniciar la sesion R o cargarlo explicitamente:

```r
readRenviron(".Renviron")
```

## 2. Preparar R y comprobar Python

`reticulate` debe estar instalado en la biblioteca persistente del proyecto:

```r
install.packages("reticulate", repos = "https://cloud.r-project.org")
```

Confirmar que no se esta usando un Python descargado por `reticulate`:

```r
reticulate::py_config()
```

La linea `python:` debe coincidir con `RETICULATE_PYTHON`.

## 3. Abrir la conexion S3

```r
source("R/almacenamiento/s3_io.R")
cliente_s3 <- obtener_cliente_s3()
class(cliente_s3)
```

La clase esperada comienza con `botocore.client.S3`. Crear el cliente no sube
ni descarga datos.

## 4. Probar permisos de escritura y lectura

Crear un objeto pequeno con nombre unico:

```r
ruta_prueba <- paste0(
  "_pruebas/prueba_escritura_",
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  ".txt"
)

key_prueba <- escribir_texto_s3(
  cliente_s3,
  ruta_prueba,
  paste("Prueba de escritura desde Cloudera:", Sys.time())
)
```

Comprobar existencia y contenido:

```r
existe_objeto_s3(cliente_s3, ruta_prueba)
leer_texto_s3(cliente_s3, ruta_prueba)
```

Ambas operaciones deben finalizar sin error y la primera debe devolver `TRUE`.

## 5. Subir un archivo

Las rutas relativas se agregan debajo de `ASIGNA_FLUJOS_S3_PREFIX`:

```r
ruta_local <- "data/published/catalogo/fecha=2026-05-20.yml"
ruta_relativa <- "published/catalogo/fecha=2026-05-20.yml"

subir_archivo_s3(
  cliente_s3,
  ruta_local,
  ruta_relativa
)
```

La clave resultante tiene esta forma:

```text
<ASIGNA_FLUJOS_S3_PREFIX>/published/catalogo/fecha=2026-05-20.yml
```

## 6. Preparar una subida completa de `data/published`

Construir primero el plan sin transferir archivos:

```r
raiz_local <- normalizePath(
  "data/published", winslash = "/", mustWork = TRUE
)

archivos_locales <- list.files(
  raiz_local,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)
archivos_locales <- archivos_locales[!dir.exists(archivos_locales)]

rutas_internas <- substring(
  normalizePath(archivos_locales, winslash = "/", mustWork = TRUE),
  nchar(raiz_local) + 2L
)

rutas_s3 <- vapply(
  rutas_internas,
  function(ruta) unir_key_s3("published", ruta),
  character(1L)
)

plan_subida <- data.frame(
  local = archivos_locales,
  destino_s3 = rutas_s3,
  bytes = file.info(archivos_locales)$size,
  stringsAsFactors = FALSE
)

c(
  archivos = nrow(plan_subida),
  megabytes = round(sum(plan_subida$bytes) / 1024^2, 2)
)
```

Los archivos ocultos como `.gitkeep` quedan excluidos.

Antes de subir, revisar si ya existen objetos:

```r
objetos_existentes <- listar_objetos_s3(cliente_s3, "published/")
length(objetos_existentes)
head(objetos_existentes)
```

## 7. Ejecutar la subida completa

La operacion sobrescribe cualquier objeto que ya tenga la misma clave. Revisar
el prefijo y los objetos existentes antes de continuar.

```r
estado_subida <- rep("pendiente", nrow(plan_subida))
detalle_subida <- rep("", nrow(plan_subida))

for (i in seq_len(nrow(plan_subida))) {
  tryCatch(
    {
      subir_archivo_s3(
        cliente_s3,
        plan_subida$local[[i]],
        plan_subida$destino_s3[[i]]
      )
      estado_subida[[i]] <- "ok"
    },
    error = function(error) {
      estado_subida[[i]] <<- "error"
      detalle_subida[[i]] <<- conditionMessage(error)
    }
  )

  if (i %% 10L == 0L || i == nrow(plan_subida)) {
    message(sprintf(
      "Procesados %d de %d; errores: %d",
      i,
      nrow(plan_subida),
      sum(estado_subida == "error")
    ))
  }
}

table(estado_subida)
```

## 8. Verificar que no falten objetos

```r
keys_s3_publicadas <- listar_objetos_s3(cliente_s3, "published/")

keys_esperadas <- vapply(
  plan_subida$destino_s3,
  key_absoluta_s3,
  character(1L)
)

c(
  esperadas = length(keys_esperadas),
  encontradas = length(keys_s3_publicadas),
  faltantes = length(setdiff(keys_esperadas, keys_s3_publicadas)),
  inesperadas = length(setdiff(keys_s3_publicadas, keys_esperadas))
)
```

Para comprobar contenido, descargar una muestra y comparar MD5:

```r
copia_temporal <- tempfile()
descargar_archivo_s3(cliente_s3, ruta_relativa, copia_temporal)

identical(
  unname(tools::md5sum(ruta_local)),
  unname(tools::md5sum(copia_temporal))
)
```

## 9. Descargar un archivo

```r
descargar_archivo_s3(
  cliente_s3,
  "published/catalogo/fecha=2026-05-20.yml",
  "data/published/catalogo/fecha=2026-05-20.yml"
)
```

La funcion crea automaticamente los directorios locales necesarios.

## 10. Descargar toda la publicacion

Primero listar y revisar las claves. Despues reconstruir `data/published`:

```r
keys_publicadas <- listar_objetos_s3(cliente_s3, "published/")
prefix_base <- key_absoluta_s3("")

rutas_relativas <- if (nzchar(prefix_base)) {
  substring(keys_publicadas, nchar(prefix_base) + 2L)
} else {
  keys_publicadas
}

destinos_locales <- file.path("data", rutas_relativas)

for (i in seq_along(rutas_relativas)) {
  descargar_archivo_s3(
    cliente_s3,
    rutas_relativas[[i]],
    destinos_locales[[i]]
  )
}
```

No ejecutar esta descarga sobre una publicacion local que contenga cambios no
respaldados, porque los archivos con el mismo nombre seran reemplazados.

## 11. Problemas comunes

### `reticulate` descarga otro Python

Definir `RETICULATE_PYTHON` en `.Renviron` y reiniciar la sesion. Si el Python
incorrecto ya fue inicializado, no puede cambiarse dentro de esa misma sesion.

### `No module named 'boto3'`

Confirmar que `reticulate::py_config()` usa el Python de Cloudera. No instalar
`boto3` en el Python aislado antes de comprobar la ruta del runtime.

### La lectura funciona pero la escritura falla

Los permisos `GetObject` y `PutObject` son independientes. Ejecutar primero la
prueba pequena de la seccion 4 y solicitar permisos sobre el prefijo correcto si
la escritura es rechazada.

### La aplicacion Shiny no usa los objetos recien subidos

La subida a S3 no cambia automaticamente la fuente de Shiny. Mientras
`ASIGNA_FLUJOS_ENV=local`, la aplicacion sigue leyendo `data/published` dentro
del proyecto. La sincronizacion de S3 al cache local se implementa por separado.

## Validacion realizada

El 28 de octubre de 2026 se comprobo en Cloudera:

- conexion R mediante `reticulate` y `cml.data_v1`;
- escritura, existencia, lectura y comparacion MD5;
- subida de 282 archivos operativos, aproximadamente 257 MB;
- 0 claves faltantes y 0 claves inesperadas bajo `published/`.
