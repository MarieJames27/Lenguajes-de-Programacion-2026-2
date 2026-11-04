#lang racket

;; Interfaz del entorno. El resto del intérprete hace require de este módulo,
;; nunca de la implementación.

;; denval? viene de la implementación de los valores y no de vals.rkt, porque
;; los tipos son mutuamente recursivos: un procedimiento es un valor denotado y
;; guarda un entorno. Si las interfaces se importaran entre sí, los require
;; formarían un ciclo, y Racket no lo admite.
(require "vals-impl.rkt"
         "env-impl-struct.rkt")

(provide
 (contract-out
  [env? (-> any/c boolean?)]
  [empty-env (-> env?)]
  [apply-env (-> env? symbol? denval?)]
  [extend-env (-> symbol? denval? env? env?)]

  [list->env (-> (listof (cons/c symbol? denval?))
                 env?)]))

(define (list->env lis)
  (if (null? lis)
      (empty-env)
      (extend-env (car (first lis))
                  (cdr (first lis))
                  (list->env (rest lis)))))
