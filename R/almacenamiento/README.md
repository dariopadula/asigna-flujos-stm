# Almacenamiento compartido

Esta carpeta contiene utilidades de acceso a almacenamiento que pueden ser
reutilizadas por los pipelines y por la aplicacion Shiny.

La integracion con S3 vive en `s3_io.R`. La configuracion del ambiente se
recibe mediante las variables `ASIGNA_FLUJOS_S3_CONNECTION`,
`ASIGNA_FLUJOS_S3_BUCKET` y `ASIGNA_FLUJOS_S3_PREFIX`; este directorio no debe
contener credenciales, nombres privados de buckets ni otros secretos.

El módulo no abre conexiones al cargarse. Los consumidores deben cargarlo con
`source()` y solicitar el cliente mediante `obtener_cliente_s3()` cuando lo
necesiten. Las conexiones también pueden inyectarse, lo que permite probar sin
red ni credenciales.

Las operaciones de prefijo separan planificación, transferencia, validación y
promoción al caché. Las subidas verifican tamaños y rechazan objetos existentes
salvo que el llamador solicite comprobar y reutilizar contenido idéntico. Los
lanzadores del pipeline materializan referencias y registros S3 en el caché
local antes de invocar la lógica existente.

La operativa comprobada de configuracion, subida, descarga y validacion esta
documentada en [GUIA_S3_CLOUDERA.md](GUIA_S3_CLOUDERA.md).
