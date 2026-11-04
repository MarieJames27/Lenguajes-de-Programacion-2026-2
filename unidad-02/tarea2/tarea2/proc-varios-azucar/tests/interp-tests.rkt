#lang racket

;; Pruebas de interp.rkt: value-of

(require rackunit
         "../ast.rkt"
         "../vals.rkt"
         "../env.rkt"
         "../procs.rkt"
         "../interp.rkt")

(define ρ0 (empty-env))
(define ρx (extend-env 'x (intval 7) ρ0))

;; Los constructos base: una prueba por regla, más el borde

(check-equal? (value-of (const-exp 5) ρ0)
              (intval 5)
              "[const] un número vale lo que dice")

(check-equal? (value-of (var-exp 'x) ρx)
              (intval 7)
              "[var] un nombre vale lo que el entorno le da")

(check-equal? (value-of (diff-exp (const-exp 10) (const-exp 3)) ρ0)
              (intval 7)
              "[diff] la resta de los valores de las partes")

(check-equal? (value-of (zero?-exp (const-exp 0)) ρ0)
              (boolval #t)
              "[zero?-t] cero da verdadero")

(check-equal? (value-of (zero?-exp (const-exp 5)) ρ0)
              (boolval #f)
              "[zero?-f] distinto de cero da falso")

(check-equal? (value-of (if-exp (zero?-exp (const-exp 0))
                                (const-exp 1)
                                (const-exp 2))
                        ρ0)
              (intval 1)
              "[if-t] con condición verdadera vale la rama then")

(check-equal? (value-of (if-exp (zero?-exp (const-exp 9))
                                (const-exp 1)
                                (const-exp 2))
                        ρ0)
              (intval 2)
              "[if-f] con condición falsa vale la rama else")

(check-equal? (value-of (let-exp 'y (const-exp 5) (var-exp 'y)) ρ0)
              (intval 5)
              "[let] el cuerpo ve la vinculación nueva")

(check-exn #rx"la variable y no está vinculada"
           (lambda () (value-of (var-exp 'y) ρx))
           "borde: un nombre que nadie vinculó no tiene valor")

;; Lo que cada regla dice y una sola prueba no muestra

(check-equal? (value-of (const-exp -5) ρ0)
              (intval -5)
              "[const] la constante puede ser negativa")

(check-equal? (value-of (var-exp 'b) (extend-env 'b (boolval #f) ρ0))
              (boolval #f)
              "[var] regresa lo que haya, también un booleano")

(check-equal? (value-of (diff-exp (const-exp 3) (const-exp 10)) ρ0)
              (intval -7)
              "[diff] la resta no es valor absoluto")

(check-equal? (value-of (diff-exp (var-exp 'x) (const-exp 2)) ρx)
              (intval 5)
              "[diff] el entorno baja a las subexpresiones")

(check-equal? (value-of (if-exp (zero?-exp (const-exp 0))
                                (const-exp 1)
                                (var-exp 'nope))
                        ρ0)
              (intval 1)
              "[if] solo se evalúa la rama elegida: nope no está vinculada y no truena")

(check-equal? (value-of (let-exp 'y (const-exp 5)
                          (diff-exp (var-exp 'y) (var-exp 'x)))
                        ρx)
              (intval -2)
              "[let] el cuerpo ve ρ más la vinculación nueva")

(check-equal? (value-of (let-exp 'x (const-exp 42) (var-exp 'x)) ρx)
              (intval 42)
              "[let] eclipsado: la vinculación nueva tapa a la vieja")

(check-equal? (value-of (let-exp 'x (diff-exp (var-exp 'x) (const-exp 1))
                          (var-exp 'x))
                        ρx)
              (intval 6)
              "[let] e1 se evalúa en ρ, no en ρ'")

(check-exn #rx"la variable y no está vinculada"
           (lambda ()
             (value-of (diff-exp (let-exp 'y (const-exp 5) (var-exp 'y))
                                 (var-exp 'y))
                       ρ0))
           "[let] la vinculación no sale del cuerpo")

;; Errores de tipo: son del extractor, no del contrato

(check-exn #rx"val->int: Esperaba un entero"
           (lambda ()
             (value-of (diff-exp (const-exp 1) (zero?-exp (const-exp 0))) ρ0))
           "restar un booleano")

(check-exn #rx"val->int: Esperaba un entero"
           (lambda ()
             (value-of (zero?-exp (zero?-exp (const-exp 0))) ρ0))
           "preguntar si un booleano es cero")

(check-exn #rx"val->bool: Esperaba un booleano"
           (lambda ()
             (value-of (if-exp (const-exp 1) (const-exp 2) (const-exp 3)) ρ0))
           "un entero como condición: PROC no hace magia")

;; El contrato de value-of

(check-exn exn:fail:contract?
           (lambda () (value-of 5 ρ0))
           "el primer argumento tiene que ser una expresión")

(check-exn exn:fail:contract?
           (lambda () (value-of (const-exp 5) "hola"))
           "el segundo argumento tiene que ser un entorno")

;; Las seis cláusulas base juntas

(define exp1
  (let-exp 'z (const-exp -420)
    (let-exp 'x (const-exp 666)
      (diff-exp
       (let-exp 'x (const-exp 42)
         (diff-exp
          (var-exp 'z)
          (let-exp 'w (const-exp 7)
            (diff-exp (var-exp 'w) (var-exp 'x)))))
       (var-exp 'x)))))

(check-equal? (value-of exp1 ρ0)
              (intval -1051)
              "cuatro let anidados, dos eclipsados, tres restas")

;; Aritmética: una prueba por regla, más los bordes

(check-equal? (value-of (minus-exp (const-exp 5)) ρ0)
              (intval -5)
              "[minus] el negativo del valor")

(check-equal? (value-of (mul-exp (const-exp 3) (const-exp 4)) ρ0)
              (intval 12)
              "[mul] el producto de los valores de las partes")

(check-equal? (value-of (quotient-exp (const-exp 7) (const-exp 2)) ρ0)
              (intval 3)
              "[quotient] el cociente entero de los valores de las partes")

(check-exn #rx"^quotient:"
           (lambda () (value-of (quotient-exp (const-exp 7) (const-exp 0)) ρ0))
           "borde: [quotient] entre cero la regla calla y quien se queja es Racket")

;; Lo que cada regla dice y una sola prueba no muestra

(check-equal? (value-of (minus-exp (minus-exp (const-exp 5))) ρ0)
              (intval 5)
              "[minus] dos veces el negativo es el mismo número")

(check-equal? (value-of (minus-exp (diff-exp (var-exp 'x) (const-exp 10))) ρx)
              (intval 3)
              "[minus] el entorno baja a la subexpresión")

(check-equal? (value-of (mul-exp (const-exp -3) (const-exp 4)) ρ0)
              (intval -12)
              "[mul] con signo")

(check-equal? (value-of (quotient-exp (const-exp -7) (const-exp 2)) ρ0)
              (intval -3)
              "[quotient] trunca hacia cero: -7 entre 2 es -3, no -4")

(check-equal? (value-of (quotient-exp (const-exp 7) (const-exp -2)) ρ0)
              (intval -3)
              "[quotient] trunca hacia cero también con el divisor negativo")

;; Errores de tipo en la aritmética: también del extractor

(check-exn #rx"val->int: Esperaba un entero"
           (lambda () (value-of (minus-exp (zero?-exp (const-exp 0))) ρ0))
           "el negativo de un booleano")

(check-exn #rx"val->int: Esperaba un entero"
           (lambda ()
             (value-of (mul-exp (const-exp 2) (zero?-exp (const-exp 0))) ρ0))
           "multiplicar un booleano")

;; Booleanos: una prueba por regla

(define tt (zero?-exp (const-exp 0)))   ; vale #t
(define ff (zero?-exp (const-exp 1)))   ; vale #f

(check-equal? (value-of (equal?-exp (const-exp 3) (const-exp 3)) ρ0)
              (boolval #t)
              "[equal?-t] iguales da verdadero")

(check-equal? (value-of (equal?-exp (const-exp 3) (const-exp 4)) ρ0)
              (boolval #f)
              "[equal?-f] distintos da falso")

(check-equal? (value-of (less?-exp (const-exp 3) (const-exp 4)) ρ0)
              (boolval #t)
              "[less?-t] el primero menor da verdadero")

(check-equal? (value-of (less?-exp (const-exp 3) (const-exp 3)) ρ0)
              (boolval #f)
              "[less?-f] igual no es menor")

;; equal? compara valores expresados en general, no solo números

(check-equal? (value-of (equal?-exp tt tt) ρ0)
              (boolval #t)
              "[equal?-t] dos booleanos iguales")

(check-equal? (value-of (equal?-exp tt ff) ρ0)
              (boolval #f)
              "[equal?-f] dos booleanos distintos")

(check-equal? (value-of (equal?-exp (const-exp 0) ff) ρ0)
              (boolval #f)
              "[equal?-f] un número y un booleano son distintos, y no es error")

(check-equal? (value-of (equal?-exp tt (const-exp 1)) ρ0)
              (boolval #f)
              "[equal?-f] un booleano y un número, en el otro orden")

;; El orden de less?: false, después los números, después true

(check-equal? (value-of (less?-exp (const-exp 4) (const-exp 3)) ρ0)
              (boolval #f)
              "[less?-f] el primero mayor")

(check-equal? (value-of (less?-exp ff (const-exp -1000)) ρ0)
              (boolval #t)
              "[less?-t] false es menor que cualquier número")

(check-equal? (value-of (less?-exp (const-exp 1000) tt) ρ0)
              (boolval #t)
              "[less?-t] cualquier número es menor que true")

(check-equal? (value-of (less?-exp ff tt) ρ0)
              (boolval #t)
              "[less?-t] false es menor que true")

(check-equal? (value-of (less?-exp tt (const-exp 1000)) ρ0)
              (boolval #f)
              "[less?-f] true no es menor que ningún número")

(check-equal? (value-of (less?-exp (const-exp -1000) ff) ρ0)
              (boolval #f)
              "[less?-f] ningún número es menor que false")

(check-equal? (value-of (less?-exp tt ff) ρ0)
              (boolval #f)
              "[less?-f] true no es menor que false")

(check-exn #rx"less\\?: Esperaba enteros o booleanos pero me diste list\\(\\) y 3"
           (lambda () (value-of (less?-exp (emptylist-exp) (const-exp 3)) ρ0))
           "una lista frente a un número no tiene orden: es error del intérprete")

(check-exn #rx"less\\?: Esperaba enteros o booleanos"
           (lambda () (value-of (less?-exp (emptylist-exp) (emptylist-exp)) ρ0))
           "dos listas tampoco")

(check-equal? (value-of (less?-exp ff ff) ρ0)
              (boolval #f)
              "borde: [less?-f] el orden es estricto, false no es menor que false")

(check-equal? (value-of (less?-exp tt tt) ρ0)
              (boolval #f)
              "borde: [less?-f] y true no es menor que true")

(check-equal? (value-of (if-exp (less?-exp (var-exp 'x) (const-exp 10))
                                (const-exp 1)
                                (const-exp 2))
                        ρx)
              (intval 1)
              "[less?] su valor sirve de condición, como el de zero?")

;; Listas: una prueba por regla, más los bordes

(define lista12
  (cons-exp (const-exp 1) (cons-exp (const-exp 2) (emptylist-exp))))

(check-equal? (value-of (emptylist-exp) ρ0)
              (listval '())
              "[emptylist] la lista vacía")

(check-equal? (value-of lista12 ρ0)
              (listval (list (intval 1) (intval 2)))
              "[cons] el valor de la cabeza al frente del valor de la cola")

(check-equal? (value-of (car-exp lista12) ρ0)
              (intval 1)
              "[car] el primer elemento, tal cual, sin volverlo a envolver")

(check-equal? (value-of (cdr-exp lista12) ρ0)
              (listval (list (intval 2)))
              "[cdr] lo que queda, envuelto otra vez como lista")

(check-equal? (value-of (null?-exp (emptylist-exp)) ρ0)
              (boolval #t)
              "[null?-t] la lista vacía da verdadero")

(check-equal? (value-of (null?-exp lista12) ρ0)
              (boolval #f)
              "[null?-f] una lista con algo da falso")

(check-exn #rx"^car:"
           (lambda () (value-of (car-exp (emptylist-exp)) ρ0))
           "borde: [car] de la lista vacía la regla calla y quien se queja es Racket")

(check-exn #rx"^cdr:"
           (lambda () (value-of (cdr-exp (emptylist-exp)) ρ0))
           "borde: [cdr] de la lista vacía, igual")

;; Lo que cada regla dice y una sola prueba no muestra

(check-equal? (value-of (cons-exp (emptylist-exp) (emptylist-exp)) ρ0)
              (listval (list (listval '())))
              "[cons] una lista puede guardar listas")

(check-equal? (value-of (cons-exp (zero?-exp (const-exp 0)) lista12) ρ0)
              (listval (list (boolval #t) (intval 1) (intval 2)))
              "[cons] y valores de clases distintas")

(check-equal? (value-of (null?-exp (cons-exp (emptylist-exp) (emptylist-exp))) ρ0)
              (boolval #f)
              "[null?-f] una lista cuyo único elemento es la vacía no es vacía")

(check-equal? (value-of (car-exp (cons-exp (var-exp 'x) (emptylist-exp))) ρx)
              (intval 7)
              "[car] el entorno baja a las partes del cons")

(check-equal? (value-of (let-exp 'l lista12 (car-exp (cdr-exp (var-exp 'l)))) ρ0)
              (intval 2)
              "[let] una lista vive en el entorno como cualquier valor")

;; Errores de tipo en las listas: del extractor

(check-exn #rx"val->list: Esperaba una lista"
           (lambda () (value-of (cons-exp (const-exp 1) (const-exp 2)) ρ0))
           "la cola de un cons tiene que ser una lista")

(check-exn #rx"val->list: Esperaba una lista"
           (lambda () (value-of (car-exp (const-exp 1)) ρ0))
           "car de un entero")

(check-exn #rx"val->int: Esperaba un entero"
           (lambda () (value-of (diff-exp (const-exp 1) (emptylist-exp)) ρ0))
           "restar una lista")

;; Tarea 1: una prueba por regla, más los bordes

(check-equal? (value-of (not-exp tt) ρ0)
              (boolval #f)
              "[not] el contrario de verdadero")

(check-equal? (value-of (not-exp ff) ρ0)
              (boolval #t)
              "[not] el contrario de falso")

(check-exn #rx"val->bool: Esperaba un booleano"
           (lambda () (value-of (not-exp (const-exp 5)) ρ0))
           "[not] de un entero es error del extractor")

(check-equal? (value-of (or-exp tt (car-exp (emptylist-exp))) ρ0)
              (boolval #t)
              "[or-t] con el primero verdadero vale verdadero y el segundo no se evalúa")

(check-equal? (value-of (or-exp ff tt) ρ0)
              (boolval #t)
              "[or-f] con el primero falso vale lo que diga el segundo, verdadero")

(check-equal? (value-of (or-exp ff ff) ρ0)
              (boolval #f)
              "[or-f] con el primero falso vale lo que diga el segundo, falso")

(check-exn #rx"val->bool: Esperaba un booleano"
           (lambda () (value-of (or-exp ff (const-exp 5)) ρ0))
           "[or-f] el segundo también tiene que ser booleano")

(check-equal? (value-of (unpack-exp '(a b) lista12 (diff-exp (var-exp 'a) (var-exp 'b))) ρ0)
              (intval -1)
              "[unpack] cada nombre se vincula al elemento que le toca por posición")

(check-equal? (value-of (unpack-exp '() (emptylist-exp) (const-exp 5)) ρ0)
              (intval 5)
              "[unpack] cero nombres contra la lista vacía")

(check-equal? (value-of (unpack-exp '(y) (cons-exp (const-exp 1) (emptylist-exp))
                                    (diff-exp (var-exp 'y) (var-exp 'x)))
                        ρx)
              (intval -6)
              "[unpack] el cuerpo ve ρ más las vinculaciones nuevas")

(check-equal? (value-of (unpack-exp '(x x) lista12 (var-exp 'x)) ρ0)
              (intval 2)
              "[unpack] con un nombre repetido gana el último")

(check-exn #rx"unpack: 3 nombres para una lista de 2 elementos"
           (lambda () (value-of (unpack-exp '(a b c) lista12 (var-exp 'a)) ρ0))
           "borde: [unpack] más nombres que elementos")

(check-exn #rx"unpack: 1 nombres para una lista de 2 elementos"
           (lambda () (value-of (unpack-exp '(a) lista12 (var-exp 'a)) ρ0))
           "borde: [unpack] menos nombres que elementos")

(check-exn #rx"val->list: Esperaba una lista"
           (lambda () (value-of (unpack-exp '(a) (const-exp 1) (var-exp 'a)) ρ0))
           "[unpack] lo que se desempaca tiene que ser una lista")

;; Procedimientos: una prueba por regla, más lo que la clausura decide

(check-equal? (value-of (proc-exp 'y (var-exp 'y)) ρx)
              (procval (closure 'y (var-exp 'y) ρx))
              "[proc] un proc vale una clausura con el parámetro, el cuerpo y el entorno")

(define resta11 (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 11))))

(check-equal? (value-of (call-exp resta11 (const-exp 77)) ρ0)
              (intval 66)
              "[call] el cuerpo se evalúa con el parámetro vinculado al argumento")

(check-equal? (value-of (call-exp resta11 (diff-exp (var-exp 'x) (const-exp 1))) ρx)
              (intval -5)
              "[call] el operando se evalúa en el entorno de la llamada")

(check-equal? (value-of (let-exp 'f (proc-exp 'y (var-exp 'x))
                                 (let-exp 'x (const-exp 100)
                                          (call-exp (var-exp 'f) (const-exp 0))))
                        ρx)
              (intval 7)
              "[call] el cuerpo ve el entorno de creación, no el de la llamada")

(check-equal? (value-of (let-exp 'x (const-exp 1)
                                 (call-exp (proc-exp 'x (var-exp 'x)) (const-exp 2)))
                        ρ0)
              (intval 2)
              "[call] el parámetro tapa la variable del mismo nombre del entorno de creación")

(define resta (proc-exp 'x (proc-exp 'y (diff-exp (var-exp 'x) (var-exp 'y)))))

(check-equal? (value-of (call-exp (call-exp resta (const-exp 10)) (const-exp 3)) ρ0)
              (intval 7)
              "[call] un procedimiento que devuelve otro: el de dentro recuerda x")

(check-exn #rx"val->proc: Esperaba un procedimiento pero me diste 3"
           (lambda () (value-of (call-exp (const-exp 3) (const-exp 4)) ρ0))
           "[call] el operador tiene que valer un procedimiento: error del extractor")

(check-exn #rx"apply-env: la variable y no está vinculada"
           (lambda () (value-of (call-exp (proc-exp 'x (var-exp 'y)) (const-exp 1)) ρ0))
           "una variable libre del cuerpo que no está en el entorno de creación es error")

;; Comparar procedimientos

(check-exn #rx"equal\\?: Esperaba enteros, booleanos o listas pero me diste <proc \\(x\\)> y <proc \\(x\\)>"
           (lambda () (value-of (equal?-exp resta11 resta11) ρ0))
           "[equal?] dos procedimientos no se comparan")

(check-exn #rx"equal\\?: Esperaba"
           (lambda () (value-of (equal?-exp (const-exp 1) resta11) ρ0))
           "[equal?] ni un procedimiento con un número")

(check-exn #rx"equal\\?: Esperaba"
           (lambda () (value-of (equal?-exp (cons-exp resta11 (emptylist-exp))
                                            (cons-exp resta11 (emptylist-exp)))
                                ρ0))
           "[equal?] ni adentro de una lista")

(check-equal? (value-of (equal?-exp (cons-exp (const-exp 1) (emptylist-exp))
                                    (cons-exp (const-exp 1) (emptylist-exp)))
                        ρ0)
              (boolval #t)
              "[equal?] las listas se siguen comparando por su contenido")

(check-exn #rx"less\\?: Esperaba enteros o booleanos pero me diste <proc \\(x\\)> y 3"
           (lambda () (value-of (less?-exp resta11 (const-exp 3)) ρ0))
           "[less?] los procedimientos no tienen orden")
