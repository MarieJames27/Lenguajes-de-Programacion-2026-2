#lang racket

;; Pruebas de desugar.rkt: del árbol con azúcar al árbol del núcleo

(require rackunit
         "../ast.rkt"
         "../parser.rkt"
         "../desugar.rkt")

;; Una regla de transformación, una prueba

(check-equal? (desugar (parse "add(1, 2)"))
              (diff-exp (const-exp 1)
                        (diff-exp (const-exp 0) (const-exp 2)))
              "[add] add(e1, e2) se traduce a -(e1, -(0, e2))")

(check-equal? (desugar (parse "true"))
              (zero?-exp (const-exp 0))
              "[true] true se traduce a zero?(0)")

(check-equal? (desugar (parse "false"))
              (zero?-exp (const-exp 1))
              "[false] false se traduce a zero?(1)")

(check-equal? (desugar (parse "greater?(1, 2)"))
              (less?-exp (const-exp 2) (const-exp 1))
              "[greater?] greater?(e1, e2) se traduce a less?(e2, e1)")

(check-equal? (desugar (parse "+()"))
              (const-exp 0)
              "[+] +() se traduce a 0")

(check-equal? (desugar (parse "+(5)"))
              (diff-exp (const-exp 5)
                        (diff-exp (const-exp 0) (const-exp 0)))
              "[+] +(e) se traduce a add(e, +()), y de ahí al núcleo")

(check-equal? (desugar (parse "+(1, 2)"))
              (diff-exp (const-exp 1)
                        (diff-exp (const-exp 0)
                                  (diff-exp (const-exp 2)
                                            (diff-exp (const-exp 0) (const-exp 0)))))
              "[+] +(e, es ...) se traduce a add(e, +(es ...)), y de ahí al núcleo")

(check-equal? (desugar (parse "*()"))
              (const-exp 1)
              "[*] *() se traduce a 1")

(check-equal? (desugar (parse "*(5)"))
              (mul-exp (const-exp 5) (const-exp 1))
              "[*] *(e) se traduce a mul(e, *())")

(check-equal? (desugar (parse "*(1, 2)"))
              (mul-exp (const-exp 1) (mul-exp (const-exp 2) (const-exp 1)))
              "[*] *(e, es ...) se traduce a mul(e, *(es ...))")

(check-equal? (desugar (parse "and(x, y)"))
              (if-exp (var-exp 'x) (var-exp 'y) (zero?-exp (const-exp 1)))
              "[and] and(e1, e2) se traduce a if e1 then e2 else false")

(check-equal? (desugar (parse "xor(x, y)"))
              (if-exp (var-exp 'x) (not-exp (var-exp 'y)) (var-exp 'y))
              "[xor] xor(e1, e2) se traduce a if e1 then not(e2) else e2")

(check-equal? (desugar (parse "list()"))
              (emptylist-exp)
              "[list] list() se traduce a emptylist")

(check-equal? (desugar (parse "list(1, 2)"))
              (cons-exp (const-exp 1) (cons-exp (const-exp 2) (emptylist-exp)))
              "[list] list(e, es ...) se traduce a cons(e, list(es ...))")

(check-equal? (desugar (parse "cond end"))
              (car-exp (emptylist-exp))
              "[cond] cond end se traduce a car(emptylist)")

(check-equal? (desugar (parse "cond zero?(x) ==> 1 end"))
              (if-exp (zero?-exp (var-exp 'x)) (const-exp 1) (car-exp (emptylist-exp)))
              "[cond] cond e1 ==> e2 bs ... end se traduce a if e1 then e2 else cond bs ... end")

(check-equal? (desugar (parse "cond zero?(x) ==> 1 true ==> 2 end"))
              (if-exp (zero?-exp (var-exp 'x))
                      (const-exp 1)
                      (if-exp (zero?-exp (const-exp 0))
                              (const-exp 2)
                              (car-exp (emptylist-exp))))
              "[cond] con dos ramas, un if dentro del otro")

;; El núcleo pasa intacto

(define core-program
  "let x = 7
   in if equal?(zero?(-(x, 7)), less?(x, 0))
      then minus(x)
      else mul(x, quotient(x, 2))")

(check-equal? (desugar (parse core-program))
              (parse core-program)
              "un árbol sin azúcar sale igual que entró")

;; desugar recorre el árbol entero

(check-equal? (desugar (parse "let x = add(1, 2) in minus(x)"))
              (let-exp 'x
                       (diff-exp (const-exp 1)
                                 (diff-exp (const-exp 0) (const-exp 2)))
                       (minus-exp (var-exp 'x)))
              "[add] el azúcar se traduce aunque esté adentro de un let")

(check-equal? (desugar (parse "add(add(1, 2), 3)"))
              (diff-exp (diff-exp (const-exp 1)
                                  (diff-exp (const-exp 0) (const-exp 2)))
                        (diff-exp (const-exp 0) (const-exp 3)))
              "[add] las partes de un add también se traducen")

(check-equal? (desugar (parse "if greater?(add(1, 2), 0) then true else false"))
              (if-exp (less?-exp (const-exp 0)
                                 (diff-exp (const-exp 1)
                                           (diff-exp (const-exp 0) (const-exp 2))))
                      (zero?-exp (const-exp 0))
                      (zero?-exp (const-exp 1)))
              "cuatro azúcares distintos en un mismo árbol")

(check-equal? (desugar (parse "cons(add(1, 2), if null?(emptylist) then cdr(x) else car(x))"))
              (cons-exp (diff-exp (const-exp 1)
                                  (diff-exp (const-exp 0) (const-exp 2)))
                        (if-exp (null?-exp (emptylist-exp))
                                (cdr-exp (var-exp 'x))
                                (car-exp (var-exp 'x))))
              "las listas son núcleo: se reconstruyen con sus partes traducidas")

(check-equal? (desugar (parse "unpack x = list(true) in or(not(x), and(x, x))"))
              (unpack-exp '(x)
                          (cons-exp (zero?-exp (const-exp 0)) (emptylist-exp))
                          (or-exp (not-exp (var-exp 'x))
                                  (if-exp (var-exp 'x)
                                          (var-exp 'x)
                                          (zero?-exp (const-exp 1)))))
              "not, or y unpack son núcleo: se reconstruyen con sus partes traducidas")

;; El contrato de desugar

(check-exn exn:fail:contract?
           (lambda () (desugar 5))
           "desugar recibe una expresión")

;; Procedimientos

(check-equal? (desugar (parse "proc (x) add(x, 1)"))
              (proc-exp 'x (diff-exp (var-exp 'x)
                                     (diff-exp (const-exp 0) (const-exp 1))))
              "proc es núcleo: se reconstruye con el cuerpo traducido")

(check-equal? (desugar (parse "(f true)"))
              (call-exp (var-exp 'f) (zero?-exp (const-exp 0)))
              "la llamada es núcleo: se reconstruye con sus partes traducidas")

;; Varios parámetros como azúcar

(check-equal? (desugar (parse "proc (x, y) -(x, y)"))
              (parse "proc (x) proc (y) -(x, y)")
              "[proc*] dos parámetros se traducen a dos proc anidados")

(check-equal? (desugar (parse "proc (x, y, z) z"))
              (parse "proc (x) proc (y) proc (z) z")
              "[proc*] tres parámetros: la traducción se vuelve a pasar por desugar")

(check-equal? (desugar (parse "proc (x, y) add(x, y)"))
              (desugar (parse "proc (x) proc (y) -(x, -(0, y))"))
              "[proc*] el cuerpo también se traduce")

(check-equal? (desugar (parse "(f 1 2)"))
              (parse "((f 1) 2)")
              "[call*] dos operandos se traducen a dos llamadas anidadas")

(check-equal? (desugar (parse "(f 1 2 3)"))
              (parse "(((f 1) 2) 3)")
              "[call*] tres operandos: la traducción se vuelve a pasar por desugar")

(check-equal? (desugar (parse "(f true 2)"))
              (desugar (parse "((f zero?(0)) 2)"))
              "[call*] los operandos también se traducen")

(check-equal? (desugar (parse "(proc (a, b) a 1 2)"))
              (parse "((proc (a) proc (b) a 1) 2)")
              "[proc*] y [call*] juntos")
