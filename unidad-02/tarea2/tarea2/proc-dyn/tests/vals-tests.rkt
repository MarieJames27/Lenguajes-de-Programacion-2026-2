#lang racket

;; Pruebas de vals.rkt: los valores expresados y denotados

(require rackunit
         "../vals.rkt"
         "../main.rkt")

;; Igualdad de valores

(check-equal? (intval 5) (intval 5)
              "dos cajitas con el mismo entero son iguales")

;; Constructores: el contrato de entrada rechaza lo que no es del tipo

(check-exn exn:fail:contract?
           (lambda () (intval #t))
           "un booleano no entra en intval")

(check-exn exn:fail:contract?
           (lambda () (intval 6.0))
           "6.0 no es un entero exacto")

(check-exn exn:fail:contract?
           (lambda () (boolval 0))
           "el cero no es falso, no hay conversión")

;; Predicados: responden, no fallan

(check-true (expval? (intval 5))
            "un entero de PROC es un valor expresado")

(check-true (expval? (boolval #f))
            "un booleano de PROC es un valor expresado")

(check-false (expval? 5)
             "un entero de Racket no es un valor de PROC")

(check-true (denval? (boolval #t))
            "lo expresable es denotable")

(check-false (denval? "hola")
             "PROC no tiene cadenas")

;; Extractores: ida y vuelta, y el caso cruzado

(check-equal? (val->int (intval 5)) 5
              "del entero de PROC sale el entero de Racket")

(check-equal? (val->bool (boolval #f)) #f
              "#f es un valor válido, no una falla")

(check-exn #rx"Esperaba un entero"
           (lambda () (val->int (boolval #t)))
           "el error del caso cruzado es del extractor")

(check-exn #rx"Esperaba un booleano"
           (lambda () (val->bool (intval 0)))
           "simétrico")

(check-exn #rx"Esperaba un entero"
           (lambda () (val->int 5))
           "any/c deja pasar un valor de Racket y el extractor lo rechaza")

;; Listas

(check-equal? (listval (list (intval 1) (intval 2)))
              (listval (list (intval 1) (intval 2)))
              "dos listas con los mismos valores son iguales")

(check-exn exn:fail:contract?
           (lambda () (listval (list 1 2)))
           "adentro de una lista de PROC van valores de PROC, no enteros de Racket")

(check-exn exn:fail:contract?
           (lambda () (listval 5))
           "una lista de PROC guarda una lista de Racket")

(check-true (expval? (listval '()))
            "la lista vacía es un valor expresado")

(check-true (denval? (listval (list (boolval #t))))
            "una lista es denotable: puede vivir en el entorno")

(check-equal? (val->list (listval (list (intval 1)))) (list (intval 1))
              "de la lista de PROC sale la lista de Racket, con sus cajitas")

(check-equal? (val->list (listval '())) '()
              "la lista vacía sale como la lista vacía de Racket")

(check-exn #rx"Esperaba una lista"
           (lambda () (val->list (intval 5)))
           "el error del caso cruzado es del extractor")

(check-exn #rx"Esperaba un entero pero me diste list\\(1, true\\)"
           (lambda () (val->int (listval (list (intval 1) (boolval #t)))))
           "el mensaje escribe el valor en la sintaxis del lenguaje")

;; Cómo se escribe cada valor

(check-equal? (expval->string (intval -6))
              "-6"
              "un entero se escribe como número")

(check-equal? (map expval->string (list (boolval #t) (boolval #f)))
              '("true" "false")
              "un booleano se escribe como las constantes del lenguaje")

(check-equal? (expval->string (listval (list (intval 1) (boolval #t) (listval '()))))
              "list(1, true, list())"
              "una lista se escribe con list, también la vacía y las anidadas")

;; Procedimientos

;; El procedimiento sale de run, para no depender de su representación
(define id (run "proc (x) x"))

(check-true (procval? id)
            "un proc vale un procval")

(check-true (expval? id)
            "un procedimiento es un valor expresado")

(check-true (denval? id)
            "y es denotable: puede vivir en el entorno")

(check-not-exn (lambda () (val->proc id))
               "de un procval sale el procedimiento")

(check-exn exn:fail:contract?
           (lambda () (procval 5))
           "en un procval no va cualquier cosa")

(check-exn #rx"val->proc: Esperaba un procedimiento pero me diste 3"
           (lambda () (val->proc (intval 3)))
           "el error del caso cruzado es del extractor")

(check-equal? (expval->string id)
              "<proc (x)>"
              "un procedimiento se escribe con su parámetro, sin cuerpo ni entorno")

(check-equal? (expval->string (listval (list id)))
              "list(<proc (x)>)"
              "también adentro de una lista")
