#lang racket

;; Árbol de sintaxis abstracta: un struct por producción

(provide
 (contract-out
  [expression? (-> any/c boolean?)]
  [struct const-exp ([n exact-integer?])]
  [struct var-exp ([x symbol?])]
  [struct diff-exp ([e1 expression?]
                    [e2 expression?])]
  [struct zero?-exp ([e1 expression?])]
  [struct if-exp ([e1 expression?]
                  [e2 expression?]
                  [e3 expression?])]
  [struct let-exp ([x symbol?]
                   [e1 expression?]
                   [e2 expression?])]

  ;; Aritmética
  [struct minus-exp ([e1 expression?])]
  [struct add-exp ([e1 expression?]
                   [e2 expression?])]
  [struct mul-exp ([e1 expression?]
                   [e2 expression?])]
  [struct quotient-exp ([e1 expression?]
                        [e2 expression?])]

  ;; Booleanos
  [struct equal?-exp ([e1 expression?]
                      [e2 expression?])]
  [struct less?-exp ([e1 expression?]
                     [e2 expression?])]
  [struct greater?-exp ([e1 expression?]
                        [e2 expression?])]
  [struct true-exp ()]
  [struct false-exp ()]

  ;; n-arias
  [struct plus-exp ([es (listof expression?)])]
  [struct times-exp ([es (listof expression?)])]

  ;; Listas
  [struct emptylist-exp ()]
  [struct cons-exp ([e1 expression?]
                    [e2 expression?])]
  [struct car-exp ([e1 expression?])]
  [struct cdr-exp ([e1 expression?])]
  [struct null?-exp ([e1 expression?])]

  ;; Tarea 1
  [struct not-exp ([e1 expression?])]
  [struct and-exp ([e1 expression?]
                   [e2 expression?])]
  [struct cond-exp ([es1 (listof expression?)]
                    [es2 (listof expression?)])]
  [struct list-exp ([es (listof expression?)])]
  [struct unpack-exp ([xs (listof symbol?)]
                      [e1 expression?]
                      [e2 expression?])]
  [struct or-exp ([e1 expression?]
                  [e2 expression?])]
  [struct xor-exp ([e1 expression?]
                   [e2 expression?])]

  ;; Procedimientos
  [struct proc-exp ([x symbol?]
                    [e1 expression?])]
  [struct call-exp ([e1 expression?]
                    [e2 expression?])]))

(struct const-exp (n) #:transparent)
(struct var-exp (x) #:transparent)
(struct diff-exp (e1 e2) #:transparent)
(struct zero?-exp (e1) #:transparent)
(struct if-exp (e1 e2 e3) #:transparent)
(struct let-exp (x e1 e2) #:transparent)

;; Aritmética
(struct minus-exp (e1) #:transparent)
(struct add-exp (e1 e2) #:transparent)
(struct mul-exp (e1 e2) #:transparent)
(struct quotient-exp (e1 e2) #:transparent)

;; Booleanos
(struct equal?-exp (e1 e2) #:transparent)
(struct less?-exp (e1 e2) #:transparent)
(struct greater?-exp (e1 e2) #:transparent)
(struct true-exp () #:transparent)
(struct false-exp () #:transparent)

;; n-arias
(struct plus-exp (es) #:transparent)
(struct times-exp (es) #:transparent)

;; Listas
(struct emptylist-exp () #:transparent)
(struct cons-exp (e1 e2) #:transparent)
(struct car-exp (e1) #:transparent)
(struct cdr-exp (e1) #:transparent)
(struct null?-exp (e1) #:transparent)

;; Tarea 1
(struct not-exp (e1) #:transparent)
(struct and-exp (e1 e2) #:transparent)
(struct cond-exp (es1 es2) #:transparent)
(struct list-exp (es) #:transparent)
(struct unpack-exp (xs e1 e2) #:transparent)
(struct or-exp (e1 e2) #:transparent)
(struct xor-exp (e1 e2) #:transparent)

;; Procedimientos
(struct proc-exp (x e1) #:transparent)
(struct call-exp (e1 e2) #:transparent)

(define (expression? obj)
  (or (const-exp? obj)
      (var-exp? obj)
      (diff-exp? obj)
      (zero?-exp? obj)
      (if-exp? obj)
      (let-exp? obj)
      ;; Aritmética
      (minus-exp? obj)
      (add-exp? obj)
      (mul-exp? obj)
      (quotient-exp? obj)
      ;; Booleanos
      (equal?-exp? obj)
      (less?-exp? obj)
      (greater?-exp? obj)
      (true-exp? obj)
      (false-exp? obj)
      ;; n-arias
      (plus-exp? obj)
      (times-exp? obj)
      ;; Listas
      (emptylist-exp? obj)
      (cons-exp? obj)
      (car-exp? obj)
      (cdr-exp? obj)
      (null?-exp? obj)
      ;; Tarea 1
      (not-exp? obj)
      (and-exp? obj)
      (cond-exp? obj)
      (list-exp? obj)
      (unpack-exp? obj)
      (or-exp? obj)
      (xor-exp? obj)
      ;; Procedimientos
      (proc-exp? obj)
      (call-exp? obj)))
