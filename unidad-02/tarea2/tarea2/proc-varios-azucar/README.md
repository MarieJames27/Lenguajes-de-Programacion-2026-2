# proc-varios-azucar

Parte 1.2 de la tarea 2: procedimientos de varios parámetros, como azúcar sobre la
currificación. El código es el de `proc-02-procedimientos`. Las pruebas de `tests/` ya
esperan `proc (x, y, ...) e` y `(f a b ...)` con dos o más, con los `struct`
`(proc*-exp xs e1)` y `(call*-exp e1 es)`, y su traducción a `proc` y llamadas de uno.

Archivos que se modifican: `ast.rkt`, `parser.rkt` y `desugar.rkt`.

Tal como se entrega, `make test` se detiene en `parser-tests.rkt`, porque `proc*-exp` no
existe todavía. Al terminar: `373 tests passed`.
