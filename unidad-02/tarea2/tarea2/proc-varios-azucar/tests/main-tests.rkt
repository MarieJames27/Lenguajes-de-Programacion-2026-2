#lang racket

;; Pruebas de main.rkt: run y repl

(require rackunit
         "../vals.rkt"
         "../main.rkt")

;; run: del texto al valor

(check-equal? (run "let x = 7 in -(x, 2)")
              (intval 5)
              "un let con resta")

(check-equal? (run "zero?(0)")
              (boolval #t)
              "un programa puede valer un booleano")

(check-equal? (run "if zero?(-(3, 3)) then 10 else 20")
              (intval 10)
              "condición calculada")

(check-equal? (run "let y = 10 in if zero?(-(y, 10)) then 1 else 0")
              (intval 1)
              "una condición calculada sobre una variable")

;; El mismo programa que exp1 en interp-tests.rkt, como texto.
(define prog1
  "let z = -420
   in let x = 666
      in -(let x = 42
           in -(z,
                let w = 7
                in -(w, x)),
           x)")

(check-equal? (run prog1)
              (intval -1051)
              "cuatro let anidados, como texto con saltos de línea")

(check-exn #rx"la variable x no está vinculada"
           (lambda () (run "x"))
           "run evalúa en el entorno vacío: nada viene vinculado de fábrica")

;; Cada etapa se queja de lo suyo

(check-exn #rx"^string:1:2: lexer:"
           (lambda () (run "3 @ 4"))
           "un carácter que no es de PROC lo rechaza el lexer")

(check-exn #rx"^string:1:8: parse:"
           (lambda () (run "zero?(3 3"))
           "tokens en un orden que la gramática no admite los rechaza el parser")

(check-exn #rx"^apply-env:"
           (lambda () (run "-(y, 1)"))
           "un nombre sin vinculación lo rechaza el entorno, desde value-of")

(check-exn #rx"^val->int:"
           (lambda () (run "-(3, zero?(0))"))
           "un operando del tipo equivocado lo rechaza el extractor, desde value-of")

(check-exn #rx"^prog:1:8: parse:"
           (lambda () (run "zero?(3 3" 'prog))
           "run pasa la fuente al parser")

(check-exn exn:fail:contract?
           (lambda () (run 7))
           "run recibe una cadena")

;; El ciclo del REPL, con la entrada y la salida como cadenas

(define (session input)
  (with-output-to-string
    (lambda ()
      (with-input-from-string input repl))))

(check-equal? (session "zero?(0)\n")
              "==> true\n==> "
              "lee, evalúa, imprime y vuelve a pedir")

(check-equal? (session "-(1, 2)\nlet x = 7 in x\n")
              "==> -1\n==> 7\n==> "
              "el ciclo repite")

(check-equal? (session "3 @ 4\n-(3, 4)\n")
              "==> repl:1:2: lexer: carácter inesperado: @\n==> -1\n==> "
              "un error se imprime y el REPL sigue vivo")

(check-equal? (session "let x = 5\nin -(x, 1)\n")
              "==> ... 4\n==> "
              "una expresión incompleta pide otra línea")

(check-equal? (session "let x = 5\nin\n-(x,\n1)\n")
              "==> ... ... ... 4\n==> "
              "y sigue pidiendo hasta que la expresión se completa")

(check-equal? (session "let x = 5\n\nin x\n")
              "==> ... ... 5\n==> "
              "una línea vacía a la mitad de una expresión se ignora")

(check-equal? (session "let x = 5\nin in\n-(3, 4)\n")
              "==> ... repl:2:3: parse: no esperaba `in` después de `in`\n==> -1\n==> "
              "un error de sintaxis a la mitad se reporta con la línea dentro de la expresión")

(check-equal? (session "let x =\n")
              "==> ... repl:1:7: parse: el texto se acabó a la mitad de una expresión, después de `=`\n==> "
              "si la entrada se acaba a la mitad, se reporta y el REPL termina")

(check-equal? (session "list(4, 3, 1)\ncons(zero?(1), emptylist)\n")
              "==> list(4, 3, 1)\n==> list(false)\n==> "
              "el REPL escribe los valores en la sintaxis del lenguaje")

(check-equal? (session "")
              "==> "
              "el fin de la entrada termina el ciclo")

(check-equal? (session "\n-(3, 4)\n")
              "==> "
              "una línea vacía también termina, aunque haya más texto después")

;; Aritmética

(check-equal? (run "minus(-(7, 2))")
              (intval -5)
              "minus sobre una resta")

(check-equal? (run "add(3, 4)")
              (intval 7)
              "add se traduce a dos restas y vale la suma")

(check-equal? (run "let x = 4 in mul(x, add(x, 1))")
              (intval 20)
              "un add adentro de un mul adentro de un let")

(check-equal? (run "quotient(-7, 2)")
              (intval -3)
              "quotient trunca hacia cero")

(check-exn #rx"^quotient:"
           (lambda () (run "quotient(7, 0)"))
           "dividir entre cero se queja Racket, no PROC")

(check-exn #rx"^val->int:"
           (lambda () (run "add(1, zero?(0))"))
           "el error de un add habla de la resta a la que se tradujo")

;; Booleanos

(check-equal? (run "true")
              (boolval #t)
              "true vale el booleano verdadero")

(check-equal? (run "if false then 1 else 2")
              (intval 2)
              "false como condición")

(check-equal? (run "let x = 3 in if equal?(x, 3) then 1 else 0")
              (intval 1)
              "equal? con una variable")

(check-equal? (run "equal?(true, zero?(0))")
              (boolval #t)
              "equal? entre dos booleanos")

(check-equal? (run "equal?(0, false)")
              (boolval #f)
              "un número y un booleano son distintos")

(check-equal? (run "less?(false, add(1, 2))")
              (boolval #t)
              "false es menor que cualquier número")

(check-equal? (run "greater?(true, 1000)")
              (boolval #t)
              "greater? hereda el orden de less?: true es mayor que todo")

(check-equal? (run "let x = 5 in if greater?(x, 3) then -(x, 3) else -(3, x)")
              (intval 2)
              "una comparación como condición")

;; n-arias

(check-equal? (run "+(1, 2, 3, 4)")
              (intval 10)
              "+ con cuatro partes")

(check-equal? (run "+()")
              (intval 0)
              "la suma de nada es 0")

(check-equal? (run "let x = 4 in +(x, mul(x, 2), 1)")
              (intval 13)
              "+ con una multiplicación adentro")

(check-exn #rx"^val->int:"
           (lambda () (run "+(1, true)"))
           "+: una parte que no es un número")

(check-equal? (run "*(1, 2, 3, 4)")
              (intval 24)
              "* con cuatro partes")

(check-equal? (run "*()")
              (intval 1)
              "el producto de nada es 1")

(check-equal? (run "let x = 4 in *(x, add(x, 1), 2)")
              (intval 40)
              "* con una suma adentro")

(check-exn #rx"^val->int:"
           (lambda () (run "*(2, false)"))
           "*: una parte que no es un número")

;; Listas

(check-equal? (run "let x = 4 in cons(x, cons(-(x, 1), emptylist))")
              (listval (list (intval 4) (intval 3)))
              "una lista construida con el valor de una variable")

(check-equal? (run "if null?(cdr(cons(1, emptylist))) then 1 else 0")
              (intval 1)
              "la cola de una lista de uno es la vacía")

(check-exn #rx"^val->list:"
           (lambda () (run "cons(1, 2)"))
           "una cola que no es lista la rechaza el extractor")

(check-exn #rx"^car:"
           (lambda () (run "car(emptylist)"))
           "car de la vacía lo rechaza el car de Racket, no PROC")

(check-regexp-match #rx"^==> car: .*\n==> true\n==> $"
                    (session "car(emptylist)\nnull?(emptylist)\n")
                    "el error de Racket se imprime y el REPL sigue vivo")

(check-equal? (run "cons(+(1, 2), cons(greater?(2, 1), emptylist))")
              (listval (list (intval 3) (boolval #t)))
              "el azúcar adentro de un cons también se traduce")

(check-equal? (run "equal?(cons(1, emptylist), cons(1, emptylist))")
              (boolval #t)
              "equal? compara valores expresados en general, también dos listas")

;; Tarea 1

(check-equal? (run "not(zero?(0))")
              (boolval #f)
              "not de verdadero es falso")

(check-equal? (run "and(true, false)")
              (boolval #f)
              "and con un falso es falso")

(check-equal? (run "or(false, true)")
              (boolval #t)
              "or con un verdadero es verdadero")

(check-equal? (run "xor(true, true)")
              (boolval #f)
              "xor de dos verdaderos es falso")

(check-equal? (run "let x = 3 in cond zero?(x) ==> 100 zero?(-(x,3)) ==> 200 end")
              (intval 200)
              "cond vale el resultado de la primera prueba verdadera")

(check-equal? (run "let x = 4 in list(x, -(x,1), -(x,3))")
              (listval (list (intval 4) (intval 3) (intval 1)))
              "list arma la lista con los valores de sus argumentos")

(check-equal? (run "let u = 7 in unpack x y = list(u, 3) in -(x, y)")
              (intval 4)
              "unpack vincula cada nombre a un elemento de la lista")

(check-equal? (run "and(false, car(emptylist))")
              (boolval #f)
              "and no evalúa el segundo si el primero es falso")

(check-exn #rx"^car:"
           (lambda () (run "cond zero?(5) ==> 1 zero?(3) ==> 2 end"))
           "un cond sin ninguna prueba verdadera es error, por su traducción a car(emptylist)")

;; Procedimientos: los programas de EOPL §3.3

(check-equal? (run "let f = proc (x) -(x, 11)
                    in (f (f 77))")
              (intval 55)
              "un procedimiento con nombre, llamado dos veces")

(check-equal? (run "(proc (f) (f (f 77))
                     proc (x) -(x, 11))")
              (intval 55)
              "un procedimiento que recibe otro")

(check-equal? (run "let x = 200
                    in let f = proc (z) -(z, x)
                       in let x = 100
                          in let g = proc (z) -(z, x)
                             in -((f 1), (g 1))")
              (intval -100)
              "f y g salen del mismo texto y recuerdan un x distinto")

(check-equal? (session "proc (x) x\n")
              "==> <proc (x)>\n==> "
              "el REPL escribe un procedimiento con su parámetro")

(check-equal? (session "(3 4)\n")
              "==> val->proc: Esperaba un procedimiento pero me diste 3\n==> "
              "el REPL escribe el error de llamar algo que no es procedimiento")

;; Varios parámetros como azúcar

(check-equal? (run "let resta = proc (x, y) -(x, y) in (resta 10 3)")
              (intval 7)
              "un procedimiento de dos parámetros llamado con dos operandos")

(check-equal? (run "let suma3 = proc (a, b, c) +(a, b, c)
                    in let x = 1
                       in (suma3 x -(x, 2) *(x, 10))")
              (intval 10)
              "cada operando se evalúa y se vincula a su parámetro")

(check-equal? (run "let resta = proc (x, y) -(x, y)
                    in let quita10 = (resta 10)
                       in (quita10 3)")
              (intval 7)
              "con la currificación hay aplicación parcial: (resta 10) es un procedimiento")

(check-exn #rx"val->proc: Esperaba un procedimiento pero me diste 7"
           (lambda () (run "let resta = proc (x, y) -(x, y) in (resta 10 3 1)"))
           "un operando de más no es error de aridad: falla después, al llamar 7")

(check-equal? (run "let f = proc (x) proc (y) -(x, y) in (f 10 3)")
              (intval 7)
              "un procedimiento currificado se puede llamar con dos operandos")

(check-equal? (session "proc (x, y) x\n")
              "==> <proc (x)>\n==> "
              "el REPL escribe el procedimiento del núcleo, con un solo parámetro")
