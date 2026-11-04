#lang racket

;; Interfaz de los valores expresados y denotados. El resto del intérprete
;; hace require de este módulo, nunca de la implementación.

(require "vals-impl.rkt"
         "procs.rkt")

(provide
 (contract-out
  [struct intval ([n exact-integer?])]
  [struct boolval ([b boolean?])]
  [struct listval ([vs (listof expval?)])]
  [struct procval ([p closure?])]
  [expval? (-> any/c boolean?)]
  [denval? (-> any/c boolean?)]
  [val->int (-> any/c exact-integer?)]
  [val->bool (-> any/c boolean?)]
  [val->list (-> any/c (listof expval?))]
  [val->proc (-> any/c closure?)]
  ;; Cómo se escribe el valor en la sintaxis del lenguaje: 6, true, list(1, 2),
  ;; y un procedimiento con todos sus parámetros, <proc (x, y)>
  [expval->string (-> expval? string?)]))

(define (val->int obj)
  (match obj
    [(intval n)
     n]
    [_
     (error 'val->int "Esperaba un entero pero me diste ~a" (show obj))]))

(define (val->bool obj)
  (match obj
    [(boolval b)
     b]
    [_
     (error 'val->bool "Esperaba un booleano pero me diste ~a" (show obj))]))

(define (val->list obj)
  (match obj
    [(listval vs)
     vs]
    [_
     (error 'val->list "Esperaba una lista pero me diste ~a" (show obj))]))

(define (val->proc obj)
  (match obj
    [(procval p)
     p]
    [_
     (error 'val->proc "Esperaba un procedimiento pero me diste ~a" (show obj))]))

(define (expval->string v)
  (match v
    [(intval n)
     (number->string n)]
    [(boolval b)
     (if b "true" "false")]
    [(listval vs)
     (string-append "list(" (string-join (map expval->string vs) ", ") ")")]
    [(procval (closure xs body ρ))
     (format "<proc (~a)>"
             (string-join (map symbol->string xs) ", "))]))

;; Para los mensajes de error: un valor de PROC en su sintaxis, y cualquier
;; otra cosa como la escribe Racket.
(define (show obj)
  (if (expval? obj)
      (expval->string obj)
      (format "~a" obj)))
