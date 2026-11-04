#lang racket

;; Implementación del entorno con struct

(provide
 (all-defined-out))

(struct empty-env ())
(struct extend-env (x v env))

(define (env? obj)
  (or (empty-env? obj)
      (extend-env? obj)))

(define (apply-env env x)
  (match env
    [(empty-env)
     (error 'apply-env
            "la variable ~a no está vinculada" x)]
    [(extend-env y v env*)
     (if (equal? y x)
         v
         (apply-env env* x))]))
