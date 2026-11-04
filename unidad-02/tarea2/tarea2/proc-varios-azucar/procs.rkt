#lang racket

;; Procedimientos: la clausura. apply-procedure vive en interp.rkt, porque
;; evalúa el cuerpo con value-of.

(require "ast.rkt"
         "env.rkt")

(provide
 (contract-out
  ;; El parámetro, el cuerpo y el entorno en que se creó el procedimiento
  [struct closure ([x symbol?]
                   [body expression?]
                   [ρ env?])]))

(struct closure (x body ρ) #:transparent)
