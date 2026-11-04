#lang racket

;; Parser: de los tokens al árbol de sintaxis abstracta

(require parser-tools/yacc
         "lexer.rkt"
         "ast.rkt")

(provide
 (contract-out
  ;; La fuente es el nombre con que los errores identifican el texto
  [parse (->* (string?) (any/c) expression?)]
  [parse-port (-> input-port? expression?)]))

;; El puerto del que se lee y los dos últimos tokens que pidió el parser:
;; last-token es el que provocó el error, y previous-token el anterior, o #f
;; si el error está en el primero. Los actualiza parse-port.
(define port #f)
(define previous-token #f)
(define last-token #f)

(define (report-syntax-error token-ok? name value start end)
  (define message
    (cond
      [(and (eq? name 'EOF) (not previous-token))
       "parse: el texto está vacío"]
      [(eq? name 'EOF)
       (format "parse: el texto se acabó a la mitad de una expresión, después de `~a`"
               (token->text previous-token))]
      [previous-token
       (format "parse: no esperaba `~a` después de `~a`"
               (token->text last-token) (token->text previous-token))]
      [else
       (format "parse: no esperaba `~a`" (token->text last-token))]))
  (raise-read-error-at message port start end #:eof? (eq? name 'EOF)))

(define parse-tokens
  (parser
   (src-pos)
   (start expression)
   (end EOF)
   (tokens value-tokens empty-tokens)
   (error report-syntax-error)

   (grammar
    (expression
     [(NUM)
      (const-exp $1)]
     [(ID)
      (var-exp $1)]
     [(MINUS LPAREN expression COMMA expression RPAREN)
      (diff-exp $3 $5)]
     [(ZERO? LPAREN expression RPAREN)
      (zero?-exp $3)]
     [(IF expression THEN expression ELSE expression)
      (if-exp $2 $4 $6)]
     [(LET ID EQ expression IN expression)
      (let-exp $2 $4 $6)]

     ;; Aritmética
     [(MINUS-KW LPAREN expression RPAREN)
      (minus-exp $3)]
     [(ADD LPAREN expression COMMA expression RPAREN)
      (add-exp $3 $5)]
     [(MUL LPAREN expression COMMA expression RPAREN)
      (mul-exp $3 $5)]
     [(QUOTIENT LPAREN expression COMMA expression RPAREN)
      (quotient-exp $3 $5)]

     ;; Booleanos
     [(TRUE)
      (true-exp)]
     [(FALSE)
      (false-exp)]
     [(EQUAL? LPAREN expression COMMA expression RPAREN)
      (equal?-exp $3 $5)]
     [(LESS? LPAREN expression COMMA expression RPAREN)
      (less?-exp $3 $5)]
     [(GREATER? LPAREN expression COMMA expression RPAREN)
      (greater?-exp $3 $5)]

     ;; n-arias
     [(PLUS LPAREN expressions RPAREN)
      (plus-exp $3)]
     [(TIMES LPAREN expressions RPAREN)
      (times-exp $3)]

     ;; Listas
     [(EMPTYLIST)
      (emptylist-exp)]
     [(CONS LPAREN expression COMMA expression RPAREN)
      (cons-exp $3 $5)]
     [(CAR LPAREN expression RPAREN)
      (car-exp $3)]
     [(CDR LPAREN expression RPAREN)
      (cdr-exp $3)]
     [(NULL? LPAREN expression RPAREN)
      (null?-exp $3)]

     ;; Tarea 1
     [(NOT LPAREN expression RPAREN)
      (not-exp $3)]
     [(AND LPAREN expression COMMA expression RPAREN)
      (and-exp $3 $5)]
     [(OR LPAREN expression COMMA expression RPAREN)
      (or-exp $3 $5)]
     [(XOR LPAREN expression COMMA expression RPAREN)
      (xor-exp $3 $5)]
     [(COND branches END)
      (cond-exp (map car $2)
                (map cdr $2))]
     [(LIST LPAREN expressions RPAREN)
      (list-exp $3)]
     [(UNPACK identifiers EQ expression IN expression)
      (unpack-exp $2 $4 $6)]

     ;; Procedimientos
     [(PROC LPAREN parameters RPAREN expression)
      (proc-exp $3 $5)]
     [(LPAREN expression operands RPAREN)
      (call-exp $2 $3)])

    ;; Cada rama sale como un par (prueba . resultado)
    (branches
     [()
      '()]
     [(expression ARROW expression branches)
      (cons (cons $1 $3) $4)])

    (identifiers
     [()
      '()]
     [(ID identifiers)
      (cons $1 $2)])

    ;; Un parámetro o varios separados por comas; ε es un proc sin parámetros
    (parameters
     [()
      '()]
     [(nonempty-parameters)
      $1])
    (nonempty-parameters
     [(ID)
      (list $1)]
     [(ID COMMA nonempty-parameters)
      (cons $1 $3)])

    ;; Cero o más operandos, pegados y sin comas
    (operands
     [()
      '()]
     [(nonempty-operands)
      $1])
    (nonempty-operands
     [(expression)
      (list $1)]
     [(expression nonempty-operands)
      (cons $1 $2)])

    ;; Dos no terminales para que una coma colgando, +(1,), no pase
    (expressions
     [()
      '()]
     [(nonempty-expressions)
      $1])
    (nonempty-expressions
     [(expression)
      (list $1)]
     [(expression COMMA nonempty-expressions)
      (cons $1 $3)]))))

(define (parse str [source 'string])
  (define in (open-input-string str source))
  (port-count-lines! in)
  (parse-port in))

;; El puerto tiene que contar líneas (port-count-lines!) para que las
;; posiciones traigan línea y columna.
(define (parse-port in)
  (set! port in)
  (set! previous-token #f)
  (set! last-token #f)
  (parse-tokens next))

(define (next)
  (define pt (next-token port))
  (set! previous-token last-token)
  (set! last-token (position-token-token pt))
  pt)
