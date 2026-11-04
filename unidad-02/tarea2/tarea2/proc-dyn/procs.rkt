#lang racket

;; Procedimientos: la clausura. apply-procedure vive en interp.rkt, porque
;; evalúa el cuerpo con value-of.

(require "ast.rkt"
         "env.rkt")

(provide
 (contract-out
  ;; El parámetro y el cuerpo. Con vinculación dinámica no se guarda el
  ;; entorno de creación: el cuerpo se evalúa en el de la llamada.
  [struct closure ([x symbol?]
                   [body expression?])]))

(struct closure (x body) #:transparent)
