# proc-varios-nucleo

Parte 1.1 de la tarea 2: procedimientos de varios parámetros, en el núcleo. El código es el
de `proc-02-procedimientos`. Las pruebas de `tests/` ya esperan `proc (x, y) e`, `(f a b)`,
`proc () e` y `(f)`, con los `struct` `(proc-exp xs e1)` y `(call-exp e1 es)`, y el error
`número de argumentos equivocado`.

Archivos que se modifican: `ast.rkt`, `parser.rkt`, `desugar.rkt`, `procs.rkt`,
`interp.rkt` y `vals.rkt`.

Tal como se entrega, `make test` da `22/225 test failures`, y tres módulos de pruebas se
detienen porque construyen la clausura con una lista de parámetros. Al terminar:
`378 tests passed`.
