#lang racket

;; Pruebas de env.rkt: la interfaz del entorno

(require rackunit
         "../vals.rkt"
         "../env.rkt"
         "../main.rkt")

;; Las tres operaciones: las ecuaciones del entorno hechas prueba

(check-true (env? (empty-env))
            "el entorno vacío es un entorno")

(check-exn #rx"la variable x no está vinculada"
           (lambda () (apply-env (empty-env) 'x))
           "buscar en el entorno vacío es error, y el mensaje dice cuál variable")

(check-equal? (apply-env (extend-env 'x (intval 7) (empty-env)) 'x)
              (intval 7)
              "[x=v]ρ(x) = v")

(check-equal? (apply-env (extend-env 'y (intval 1)
                           (extend-env 'x (intval 7) (empty-env)))
                         'x)
              (intval 7)
              "extender no borra lo anterior: [y=w]ρ(x) = ρ(x)")

(check-equal? (apply-env (extend-env 'x (intval 42)
                           (extend-env 'x (intval 666) (empty-env)))
                         'x)
              (intval 42)
              "eclipsado: gana la vinculación de adentro")

(check-equal? (apply-env (extend-env 'x (boolval #t) (empty-env)) 'x)
              (boolval #t)
              "el entorno guarda booleanos, no solo enteros")

;; Los contratos de la interfaz

(check-exn exn:fail:contract?
           (lambda () (extend-env 'x 7 (empty-env)))
           "un 7 sin cajita no entra al entorno")

(check-exn exn:fail:contract?
           (lambda () (apply-env (empty-env) "x"))
           "el nombre tiene que ser un símbolo")

;; list->env: vive en la interfaz, así que se prueba aquí

(check-equal? (apply-env (list->env (list (cons 'x (intval 1)))) 'x)
              (intval 1)
              "una lista de un par")

(check-exn #rx"no está vinculada"
           (lambda () (apply-env (list->env '()) 'x))
           "la lista vacía da el entorno vacío")

(check-equal? (apply-env (list->env (list (cons 'x (intval 42))
                                          (cons 'x (intval 666))))
                         'x)
              (intval 42)
              "el primer par de la lista queda al frente")

(check-exn exn:fail:contract?
           (lambda () (list->env (list (cons 'x 42))))
           "falla temprano: al construir, no al consultar")

;; Un procedimiento es denotable

;; El procedimiento sale de run, para no depender de su representación
(define p (run "proc (x) x"))

(check-equal? (apply-env (extend-env 'f p (empty-env)) 'f)
              p
              "el entorno guarda procedimientos")
