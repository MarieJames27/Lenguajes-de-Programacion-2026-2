#lang racket

;; Implementación de los valores expresados y denotados. La interfaz, con los
;; contratos, es vals.rkt. Este módulo no importa nada, para que env.rkt pueda
;; tomar de aquí denval? sin pasar por vals.rkt.

(provide
 (all-defined-out))

(struct intval (n) #:transparent)
(struct boolval (b) #:transparent)
(struct listval (vs) #:transparent)
(struct procval (p) #:transparent)

(define (expval? obj)
  (or (intval? obj)
      (boolval? obj)
      (listval? obj)
      (procval? obj)))

(define (denval? obj)
  (expval? obj))
