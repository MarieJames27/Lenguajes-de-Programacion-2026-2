#lang racket
(require racket/match)

; -------------------------------
; Seccion 1: representacion
; -------------------------------

(struct num-exp (num) #:transparent)
(struct id-exp (id) #:transparent)
(struct add-exp (izq der) #:transparent)
(struct mul-exp (izq der) #:transparent)
(struct with-exp (nombre ligado cuerpo) #:transparent)

; -------------------------------
; Seccion 2: asociaciones
; -------------------------------

(define (asocs-vacias) '())

(define (extender asocs nombre valor)
  (cons (cons nombre valor) asocs))

(define (buscar nombre asocs)
  (cdr (assoc nombre asocs)))

; -------------------------------
; Seccion 2: calc
; -------------------------------

(define (calc e asocs)
  (match e
    [(num-exp n)
     n]

    [(id-exp x)
     (buscar x asocs)]

    [(add-exp e1 e2)
     (+ (calc e1 asocs)
        (calc e2 asocs))]

    [(mul-exp e1 e2)
     (* (calc e1 asocs)
        (calc e2 asocs))]

    [(with-exp x e1 e2)
     (calc e2
           (extender asocs
                     x
                     (calc e1 asocs)))]))

; -------------------------------
; Casos de verificacion
; -------------------------------

(define caso-a
  (add-exp
    (num-exp 3)
    (mul-exp
      (num-exp 2)
      (num-exp 4))))

(define caso-b
  (with-exp
    'y
    (add-exp (num-exp 3) (num-exp 4))
    (mul-exp (id-exp 'y) (id-exp 'y))))

(define caso-c
  (with-exp
    'x
    (add-exp (num-exp 2) (num-exp 3))
    (with-exp
      'y
      (mul-exp (id-exp 'x) (num-exp 2))
      (add-exp (id-exp 'x) (id-exp 'y)))))

(define caso-d
  (with-exp
    'a
    (num-exp 5)
    (mul-exp
      (with-exp
        'b
        (add-exp (id-exp 'a) (num-exp 1))
        (id-exp 'b))
      (id-exp 'a))))

(calc caso-a (asocs-vacias)) ; 11
(calc caso-b (asocs-vacias)) ; 49
(calc caso-c (asocs-vacias)) ; 15
(calc caso-d (asocs-vacias)) ; 30

#|
Trazado de:
with x = add(2, 3) in
with y = mul(x, 2) in
add(x, y)

Abreviaturas:
N2  = (num-exp 2)
N3  = (num-exp 3)
N2' = (num-exp 2)
X   = (add-exp N2 N3)
M   = (mul-exp (id-exp 'x) N2')
B   = (add-exp (id-exp 'x) (id-exp 'y))

(calc (with-exp 'x X
               (with-exp 'y M B))
      sigma0)

= (calc (with-exp 'y M B)
        extend(sigma0, 'x, (calc X sigma0)))                [with]

= (calc (with-exp 'y M B)
        extend(sigma0, 'x,
               (+ (calc N2 sigma0) (calc N3 sigma0))))     [add]

= (calc (with-exp 'y M B)
        extend(sigma0, 'x,
               (+ 2 (calc N3 sigma0))))                    [num]

= (calc (with-exp 'y M B)
        extend(sigma0, 'x,
               (+ 2 3)))                                  [num]

= (calc (with-exp 'y M B) sigma1)
  con sigma1 = extend(sigma0, 'x, 5)

= (calc B
        extend(sigma1, 'y, (calc M sigma1)))               [with]

= (calc B
        extend(sigma1, 'y,
               (* (calc (id-exp 'x) sigma1)
                  (calc N2' sigma1))))                    [mul]

= (calc B
        extend(sigma1, 'y,
               (* (buscar 'x sigma1)
                  (calc N2' sigma1))))                    [id]

= (calc B
        extend(sigma1, 'y,
               (* 5 (calc N2' sigma1))))                  [id]

= (calc B
        extend(sigma1, 'y,
               (* 5 2)))                                  [num]

= (calc B sigma2)
  con sigma2 = extend(sigma1, 'y, 10)

= (+ (calc (id-exp 'x) sigma2)
     (calc (id-exp 'y) sigma2))                            [add]

= (+ (buscar 'x sigma2)
     (calc (id-exp 'y) sigma2))                            [id]

= (+ 5 (buscar 'y sigma2))                                [id]

= (+ 5 10)

= 15
|#
