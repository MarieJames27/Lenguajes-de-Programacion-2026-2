#lang racket

;; Intérprete: value-of

(require "ast.rkt"
         "env.rkt"
         "vals.rkt"
         "procs.rkt")

(provide
 (contract-out
  [value-of (-> expression? env?
                expval?)]))

(define (value-of e ρ)
  (match e
    [(const-exp n)
     (intval n)]
    [(var-exp x)
     (apply-env ρ x)]
    [(diff-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (intval (- (val->int v1)
                (val->int v2)))]
    [(zero?-exp e1)
     (define v1 (value-of e1 ρ))
     (boolval (zero? (val->int v1)))]
    [(if-exp e1 e2 e3)
     (define v1 (value-of e1 ρ))
     (if (val->bool v1)
         (value-of e2 ρ)
         (value-of e3 ρ))]
    [(let-exp x e1 e2)
     (define v1 (value-of e1 ρ))
     (define ρ* (extend-env x v1 ρ))
     (value-of e2 ρ*)]

    ;; Aritmética
    [(minus-exp e1)
     (define v1 (value-of e1 ρ))
     (intval (- (val->int v1)))]
    [(mul-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (intval (* (val->int v1)
                (val->int v2)))]
    [(quotient-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (intval (quotient (val->int v1)
                       (val->int v2)))]

    ;; Booleanos
    [(equal?-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (boolval (val=? v1 v2))]
    [(less?-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (boolval (val<? v1 v2))]

    ;; Listas
    [(emptylist-exp)
     (listval '())]
    [(cons-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (listval (cons v1 (val->list v2)))]
    [(car-exp e1)
     (define v1 (value-of e1 ρ))
     (car (val->list v1))]
    [(cdr-exp e1)
     (define v1 (value-of e1 ρ))
     (listval (cdr (val->list v1)))]
    [(null?-exp e1)
     (define v1 (value-of e1 ρ))
     (boolval (null? (val->list v1)))]

    ;; Tarea 1
    [(not-exp e1)
     (define v1 (value-of e1 ρ))
     (boolval (not (val->bool v1)))]
    [(unpack-exp xs e1 e2)
     (define v1 (value-of e1 ρ))
     (define vs (val->list v1))
     (unless (= (length xs) (length vs))
       (error 'value-of "unpack: ~a nombres para una lista de ~a elementos"
              (length xs) (length vs)))
     (define ρ* (extend-env* xs vs ρ))
     (value-of e2 ρ*)]
    [(or-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (if (val->bool v1)
         (boolval #t)
         (boolval (val->bool (value-of e2 ρ))))]

    ;; Procedimientos
    [(proc-exp x e1)
     (procval (closure x e1 ρ))]
    [(call-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (apply-procedure (val->proc v1) v2)]))

;; El cuerpo se evalúa en el entorno de la clausura, el de su creación,
;; extendido con el parámetro. No en el entorno de la llamada.
(define (apply-procedure c v)
  (match c
    [(closure x body ρ)
     (value-of body (extend-env x v ρ))]))

;; Supone que xs y vs miden lo mismo: la cláusula de unpack lo comprueba.
(define (extend-env* xs vs ρ)
  (if (null? xs)
      ρ
      (extend-env* (rest xs) (rest vs)
                   (extend-env (first xs) (first vs) ρ))))

;; Enteros, booleanos y listas se comparan por su contenido. Un procedimiento
;; no se compara con nada.
(define (val=? v1 v2)
  (match (list v1 v2)
    [(or (list (procval _) _) (list _ (procval _)))
     (error 'equal? "Esperaba enteros, booleanos o listas pero me diste ~a y ~a"
            (expval->string v1) (expval->string v2))]
    [(list (listval vs1) (listval vs2))
     (and (= (length vs1) (length vs2))
          (andmap val=? vs1 vs2))]
    [_
     (equal? v1 v2)]))

;; false es menor que todo, true es mayor que todo, y entre números es <.
;; Una lista o un procedimiento frente a un número o a otra lista es error.
(define (val<? v1 v2)
  (match (list v1 v2)
    [(list (boolval #f) (boolval #f))
     #f]
    [(list (boolval #f) _)
     #t]
    [(list (boolval #t) _)
     #f]
    [(list _ (boolval #f))
     #f]
    [(list _ (boolval #t))
     #t]
    [(list (intval n1) (intval n2))
     (< n1 n2)]
    [_
     (error 'less? "Esperaba enteros o booleanos pero me diste ~a y ~a"
            (expval->string v1) (expval->string v2))]))
