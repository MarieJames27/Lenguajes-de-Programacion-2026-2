#lang racket

;; Traducción del azúcar sintáctica al núcleo. Las cláusulas del núcleo
;; reconstruyen el constructo con sus partes traducidas, las de azúcar
;; traducen y vuelven a pasar el resultado por desugar.

(require "ast.rkt")

(provide
 (contract-out
  [desugar (-> expression? expression?)]))

(define (desugar e)
  (match e
    ;; Núcleo
    [(const-exp n)
     e]
    [(var-exp x)
     e]
    [(diff-exp e1 e2)
     (diff-exp (desugar e1) (desugar e2))]
    [(zero?-exp e1)
     (zero?-exp (desugar e1))]
    [(if-exp e1 e2 e3)
     (if-exp (desugar e1) (desugar e2) (desugar e3))]
    [(let-exp x e1 e2)
     (let-exp x (desugar e1) (desugar e2))]

    [(minus-exp e1)
     (minus-exp (desugar e1))]
    [(mul-exp e1 e2)
     (mul-exp (desugar e1) (desugar e2))]
    [(quotient-exp e1 e2)
     (quotient-exp (desugar e1) (desugar e2))]

    [(equal?-exp e1 e2)
     (equal?-exp (desugar e1) (desugar e2))]
    [(less?-exp e1 e2)
     (less?-exp (desugar e1) (desugar e2))]

    [(emptylist-exp)
     e]
    [(cons-exp e1 e2)
     (cons-exp (desugar e1) (desugar e2))]
    [(car-exp e1)
     (car-exp (desugar e1))]
    [(cdr-exp e1)
     (cdr-exp (desugar e1))]
    [(null?-exp e1)
     (null?-exp (desugar e1))]

    [(not-exp e1)
     (not-exp (desugar e1))]
    [(or-exp e1 e2)
     (or-exp (desugar e1) (desugar e2))]
    [(unpack-exp xs e1 e2)
     (unpack-exp xs (desugar e1) (desugar e2))]

    [(proc-exp xs e1)
     (proc-exp xs (desugar e1))]
    [(call-exp e1 es)
     (call-exp (desugar e1) (map desugar es))]

    ;; Azúcar
    
    ;; add(e1, e2) ⇝ -(e1, -(0, e2))
    [(add-exp e1 e2)
     (desugar (diff-exp e1 (diff-exp (const-exp 0) e2)))]
    ;; greater?(e1, e2) ⇝ less?(e2, e1)
    [(greater?-exp e1 e2)
     (desugar (less?-exp e2 e1))]

    [(plus-exp es)
     (cond
       ;; +() ⇝ 0
       [(null? es)
        (desugar (const-exp 0))]
       ;; +(e, es ...) ⇝ add(e, +(es ...))
       [else
        (desugar (add-exp (first es)
                          (plus-exp (rest es))))])]
    [(times-exp es)
     (cond
       ;; *() ⇝ 1
       [(null? es)
        (desugar (const-exp 1))]
       ;; *(e, es ...) ⇝ mul(e, *(es ...))
       [else
        (desugar (mul-exp (first es)
                          (times-exp (rest es))))])]

    ;; and(e1, e2) ⇝ if e1 then e2 else false
    [(and-exp e1 e2)
     (desugar (if-exp e1 e2 (false-exp)))]
    [(cond-exp es1 es2)
     (if (null? es1)
         ;; cond end ⇝ car(emptylist)
         (desugar (car-exp (emptylist-exp)))
         ;; cond e1 ==> e2 bs ... end ⇝ if e1 then e2 else cond bs ... end
         (desugar (if-exp (first es1)
                          (first es2)
                          (cond-exp (rest es1) (rest es2)))))]
    [(list-exp es)
     (cond
       ;; list() ⇝ emptylist
       [(null? es)
        (desugar (emptylist-exp))]
       ;; list(e, es ...) ⇝ cons(e, list(es ...))
       [else
        (desugar (cons-exp (first es)
                           (list-exp (rest es))))])]

    ;; xor(e1, e2) ⇝ if e1 then not(e2) else e2
    [(xor-exp e1 e2)
     (desugar (if-exp e1 (not-exp e2) e2))]

    ;; true ⇝ zero?(0)   false ⇝ zero?(1)
    [(true-exp)
     (desugar (zero?-exp (const-exp 0)))]
    [(false-exp)
     (desugar (zero?-exp (const-exp 1)))]))
