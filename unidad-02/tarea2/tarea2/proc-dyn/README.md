# proc-dyn

Parte 2.2 de la tarea 2: PROC con vinculación dinámica, el cuerpo de un procedimiento se
evalúa en el entorno de la llamada. El código es el de `proc-02-procedimientos`, con el
nombre ya cambiado: `info.rkt` declara la colección `proc-dyn`, el lector arma módulos que
usan `proc-dyn/main`, y `make install` instala el paquete `proc-dyn`, que da
`#lang proc-dyn` y puede convivir con `#lang proc`.

Archivos que se modifican: `interp.rkt`, y `procs.rkt` y `vals.rkt` si cambia lo que es un
procedimiento. Las pruebas no miran adentro del procedimiento: lo obtienen con `run`.

Tal como se entrega, `make test` da `8/363 test failures`: son las pruebas cuyo resultado
cambia con la vinculación dinámica. Al terminar: `363 tests passed`.
