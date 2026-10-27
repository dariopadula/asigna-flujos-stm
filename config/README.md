# Configuracion por ambiente

La configuracion activa se compone de dos capas:

- `base.yml`: reglas comunes, nombres logicos, versiones y parametros de negocio;
- `environments/<perfil>.yml`: ubicaciones fisicas y backend del ambiente.

`cargar_configuracion.R` fusiona ambas capas. El perfil predeterminado es
`local`; puede elegirse con `ASIGNA_FLUJOS_ENV=local|cloudera`. El mismo cargador
es usado por el pipeline diario y Shiny.

## Perfil local

Todas las raices son relativas a `proyecto_flujo/` y el cargador las convierte
en rutas absolutas. Se distinguen fuente, referencias, registros, trabajo,
publicacion, cuarentena y cache.

## Perfil Cloudera

Las raices persistentes se reciben por variables de ambiente y deben ser URI
S3. Trabajo y cache se reciben tambien por variables, pero deben pertenecer al
filesystem del job. Las variables requeridas son:

- `ASIGNA_FLUJOS_SOURCE_ROOT`
- `ASIGNA_FLUJOS_REFERENCE_ROOT`
- `ASIGNA_FLUJOS_REGISTRY_ROOT`
- `ASIGNA_FLUJOS_PUBLISHED_ROOT`
- `ASIGNA_FLUJOS_QUARANTINE_ROOT`
- `ASIGNA_FLUJOS_WORK_ROOT`
- `ASIGNA_FLUJOS_CACHE_ROOT`

No se guardan credenciales ni nombres reales de buckets. La autenticacion debe
provenir de la identidad o del mecanismo autorizado para el job.

`pipeline.yml` se conserva temporalmente como configuracion historica para
trazabilidad; los lanzadores activos ya no lo consumen.

