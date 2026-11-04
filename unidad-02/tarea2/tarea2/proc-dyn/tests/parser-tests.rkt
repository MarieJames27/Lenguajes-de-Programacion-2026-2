#lang racket

;; Pruebas de parser.rkt: de la cadena al árbol

(require rackunit
         "../ast.rkt"
         "../parser.rkt")

;; Una producción, una prueba, en el orden de la gramática

(check-equal? (parse "7")
              (const-exp 7)
              "Number")

(check-equal? (parse "x")
              (var-exp 'x)
              "Identifier")

(check-equal? (parse "-(3, 4)")
              (diff-exp (const-exp 3) (const-exp 4))
              "-(Expression, Expression)")

(check-equal? (parse "zero?(0)")
              (zero?-exp (const-exp 0))
              "zero?(Expression)")

(check-equal? (parse "if zero?(0) then 1 else 2")
              (if-exp (zero?-exp (const-exp 0)) (const-exp 1) (const-exp 2))
              "if Expression then Expression else Expression")

(check-equal? (parse "let x = 7 in x")
              (let-exp 'x (const-exp 7) (var-exp 'x))
              "let Identifier = Expression in Expression")

;; La recursión de la gramática

(check-equal? (parse "-(-(1, 2), 3)")
              (diff-exp (diff-exp (const-exp 1) (const-exp 2)) (const-exp 3))
              "una resta dentro de una resta")

(check-equal? (parse "let x = zero?(0) in if x then 1 else 2")
              (let-exp 'x
                       (zero?-exp (const-exp 0))
                       (if-exp (var-exp 'x) (const-exp 1) (const-exp 2)))
              "tres producciones distintas anidadas")

;; Lo que decidió el lexer y el parser respeta

(check-equal? (parse "-5")
              (const-exp -5)
              "el negativo es una constante, no una resta")

(check-equal? (parse "-(x-1, 2)")
              (diff-exp (var-exp 'x-1) (const-exp 2))
              "x-1 es una variable")

(check-equal? (parse "let let-x = 1 in let-x")
              (let-exp 'let-x (const-exp 1) (var-exp 'let-x))
              "un identificador puede empezar como palabra reservada")

(check-equal? (parse "  7  ")
              (const-exp 7)
              "los espacios no llegan al parser")

;; Errores de sintaxis, con el token en que se atora

(check-exn #rx"parse: el texto está vacío$"
           (lambda () (parse ""))
           "el texto vacío no es una expresión")

(check-exn #rx"el texto se acabó a la mitad de una expresión, después de `4`"
           (lambda () (parse "-(3, 4"))
           "falta cerrar el paréntesis")

(check-exn #rx"no esperaba `4` después de `3`"
           (lambda () (parse "3 4"))
           "después de una expresión completa no va otra")

(check-exn #rx"no esperaba `in` después de `=`"
           (lambda () (parse "let x = in 3"))
           "falta la expresión del valor")

(check-exn #rx"no esperaba `\\)` después de `\\)`"
           (lambda () (parse "-(3, 4))"))
           "un paréntesis de más")

(check-exn #rx"no esperaba `7` después de `let`"
           (lambda () (parse "let 7 = 7 in 7"))
           "el nombre de un let tiene que ser un identificador")

;; Ubicación de los errores de sintaxis

(define (error-location thunk)
  (with-handlers ([exn:fail:read?
                   (lambda (e)
                     (define loc (first (exn:fail:read-srclocs e)))
                     (list (srcloc-source loc) (srcloc-line loc)
                           (srcloc-column loc)))])
    (thunk)))

(check-exn exn:fail:read?
           (lambda () (parse "let x = in 3"))
           "el error del parser es un error de lectura")

(check-exn exn:fail:read:eof?
           (lambda () (parse "let x = 5"))
           "el texto incompleto es un error de fin de texto")

(check-false (with-handlers ([exn:fail:read? exn:fail:read:eof?])
               (parse "let x = in 3"))
             "un error a la mitad del texto no es de fin de texto")

(check-equal? (error-location (lambda () (parse "let x =\n  in 3")))
              '(string 2 2)
              "señala el token en que se atora: el in de la segunda línea")

(check-equal? (error-location (lambda () (parse "-(3, 4" 'prog)))
              '(prog 1 6)
              "texto incompleto: señala el final, con la fuente que se le dio")

(check-exn #rx"^prog:1:8: parse: no esperaba `in` después de `=`$"
           (lambda () (parse "let x = in 3" 'prog))
           "el mensaje empieza con fuente, línea y columna")

(check-equal? (let ([port (open-input-string "let x = 7\nin -(x, 2)")])
                (port-count-lines! port)
                (parse-port port))
              (parse "let x = 7\nin -(x, 2)")
              "parse-port lee de un puerto lo mismo que parse de una cadena")

(check-exn #rx"^string:1:0: parse: no esperaba `\\)`$"
           (lambda () (parse ")"))
           "un error en el primer token no tiene anterior")

(check-exn #rx"no esperaba `2` después de `x-1`"
           (lambda () (parse "let y = 1 in x-1 2"))
           "un identificador se muestra como se escribió")

;; De quién es cada error, y el contrato de parse

(check-exn #rx"lexer: carácter inesperado"
           (lambda () (parse "3 @ 4"))
           "el error del lexer atraviesa parse sin que el parser lo tape")

(check-exn exn:fail:contract?
           (lambda () (parse 7))
           "parse recibe una cadena: 7 y \"7\" no son lo mismo")

;; Aritmética: una producción, una prueba

(check-equal? (parse "minus(5)")
              (minus-exp (const-exp 5))
              "minus(Expression)")

(check-equal? (parse "add(1, 2)")
              (add-exp (const-exp 1) (const-exp 2))
              "add(Expression, Expression)")

(check-equal? (parse "mul(2, 3)")
              (mul-exp (const-exp 2) (const-exp 3))
              "mul(Expression, Expression)")

(check-equal? (parse "quotient(7, 2)")
              (quotient-exp (const-exp 7) (const-exp 2))
              "quotient(Expression, Expression)")

(check-exn #rx"no esperaba `\\)` después de `1`"
           (lambda () (parse "add(1)"))
           "add lleva exactamente dos expresiones")

;; Booleanos: una producción, una prueba

(check-equal? (parse "true")
              (true-exp)
              "true")

(check-equal? (parse "false")
              (false-exp)
              "false")

(check-equal? (parse "equal?(2, 3)")
              (equal?-exp (const-exp 2) (const-exp 3))
              "equal?(Expression, Expression)")

(check-equal? (parse "less?(2, 3)")
              (less?-exp (const-exp 2) (const-exp 3))
              "less?(Expression, Expression)")

(check-equal? (parse "greater?(2, 3)")
              (greater?-exp (const-exp 2) (const-exp 3))
              "greater?(Expression, Expression)")

(check-equal? (parse "let truely = 1 in truely")
              (let-exp 'truely (const-exp 1) (var-exp 'truely))
              "true es palabra reservada, truely es un nombre")

;; n-arias

(check-equal? (parse "+(1, 2, 3)")
              (plus-exp (list (const-exp 1) (const-exp 2) (const-exp 3)))
              "+(Expressions): una lista de árboles")

(check-equal? (parse "+()")
              (plus-exp '())
              "+ sin argumentos es la lista vacía")

(check-equal? (parse "+(5)")
              (plus-exp (list (const-exp 5)))
              "+ de uno es una lista de uno")

(check-exn #rx"no esperaba `\\)` después de `,`"
           (lambda () (parse "+(1,)"))
           "+: después de una coma tiene que venir una expresión")

(check-equal? (parse "*(1, 2, 3)")
              (times-exp (list (const-exp 1) (const-exp 2) (const-exp 3)))
              "*(Expressions): una lista de árboles")

(check-equal? (parse "*()")
              (times-exp '())
              "* sin argumentos es la lista vacía")

(check-equal? (parse "*(5)")
              (times-exp (list (const-exp 5)))
              "* de uno es una lista de uno")

(check-exn #rx"no esperaba `\\)` después de `,`"
           (lambda () (parse "*(1,)"))
           "*: después de una coma tiene que venir una expresión")

;; Listas: una producción, una prueba

(check-equal? (parse "emptylist")
              (emptylist-exp)
              "emptylist: un struct sin campos")

(check-equal? (parse "cons(1, emptylist)")
              (cons-exp (const-exp 1) (emptylist-exp))
              "cons(Expression, Expression)")

(check-equal? (parse "car(x)")
              (car-exp (var-exp 'x))
              "car(Expression)")

(check-equal? (parse "cdr(x)")
              (cdr-exp (var-exp 'x))
              "cdr(Expression)")

(check-equal? (parse "null?(x)")
              (null?-exp (var-exp 'x))
              "null?(Expression)")

(check-equal? (parse "cons(1, cons(2, emptylist))")
              (cons-exp (const-exp 1) (cons-exp (const-exp 2) (emptylist-exp)))
              "una lista de dos es un cons dentro de un cons")

(check-exn #rx"no esperaba `\\)` después de `1`"
           (lambda () (parse "cons(1)"))
           "cons lleva dos expresiones")

;; Tarea 1: una producción, una prueba

(check-equal? (parse "not(x)")
              (not-exp (var-exp 'x))
              "not(Expression)")

(check-equal? (parse "and(x, y)")
              (and-exp (var-exp 'x) (var-exp 'y))
              "and(Expression, Expression)")

(check-equal? (parse "or(x, y)")
              (or-exp (var-exp 'x) (var-exp 'y))
              "or(Expression, Expression)")

(check-equal? (parse "xor(x, y)")
              (xor-exp (var-exp 'x) (var-exp 'y))
              "xor(Expression, Expression)")

(check-equal? (parse "cond zero?(x) ==> 1 zero?(y) ==> 2 end")
              (cond-exp (list (zero?-exp (var-exp 'x)) (zero?-exp (var-exp 'y)))
                        (list (const-exp 1) (const-exp 2)))
              "cond: las pruebas en una lista y los resultados en otra")

(check-equal? (parse "list(1, x)")
              (list-exp (list (const-exp 1) (var-exp 'x)))
              "list(Expressions)")

(check-equal? (parse "unpack x y = l in -(x, y)")
              (unpack-exp '(x y) (var-exp 'l) (diff-exp (var-exp 'x) (var-exp 'y)))
              "unpack: los nombres en una lista de símbolos")

;; Las listas vacías de la sintaxis

(check-equal? (parse "cond end")
              (cond-exp '() '())
              "un cond sin ramas es sintaxis válida")

(check-equal? (parse "list()")
              (list-exp '())
              "list sin argumentos")

(check-equal? (parse "unpack = l in 5")
              (unpack-exp '() (var-exp 'l) (const-exp 5))
              "unpack sin nombres")

(check-exn #rx"no esperaba `end` después de `==>`"
           (lambda () (parse "cond zero?(x) ==> end"))
           "a una rama de cond no le puede faltar el resultado")

;; Procedimientos

(check-equal? (parse "proc (x) -(x, 1)")
              (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 1)))
              "proc (Identifier) Expression")

(check-equal? (parse "(f 3)")
              (call-exp (var-exp 'f) (const-exp 3))
              "(Expression Expression)")

(check-equal? (parse "((f 3) 4)")
              (call-exp (call-exp (var-exp 'f) (const-exp 3)) (const-exp 4))
              "el operador puede ser otra llamada")

(check-equal? (parse "(proc (x) x 7)")
              (call-exp (proc-exp 'x (var-exp 'x)) (const-exp 7))
              "el operador puede ser un proc escrito en el lugar")

(check-equal? (parse "proc (x) proc (y) -(x, y)")
              (proc-exp 'x (proc-exp 'y (diff-exp (var-exp 'x) (var-exp 'y))))
              "el cuerpo de un proc puede ser otro proc")

(check-exn #rx"no esperaba `,` después de `x`"
           (lambda () (parse "proc (x, y) x"))
           "un proc tiene un solo parámetro")

(check-exn #rx"no esperaba `\\)` después de `f`"
           (lambda () (parse "(f)"))
           "una llamada lleva exactamente un operando")
