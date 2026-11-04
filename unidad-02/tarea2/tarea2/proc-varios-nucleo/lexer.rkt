#lang racket

;; Lexer: del texto a los tokens

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(provide
 (contract-out
  ;; Cada token sale envuelto en un position-token con su posición en el texto
  [next-token (-> input-port? position-token?)]
  ;; Los tokens de una cadena, sin posición
  [tokenize (-> string? (listof proc-token?))]
  [proc-token? (-> any/c boolean?)]
  ;; El texto que el token representa, para los mensajes de error
  [token->text (-> proc-token? string?)]
  [raise-read-error-at (->* (string? input-port? position? position?)
                             (#:eof? boolean?)
                             none/c)])
 value-tokens
 empty-tokens
 token-names
 token-name
 token-value
 (struct-out position-token)
 (struct-out position))

(define-tokens value-tokens (NUM ID))
(define-empty-tokens empty-tokens
  (MINUS ZERO? IF THEN ELSE LET EQ IN
         LPAREN RPAREN COMMA EOF
         ;; Aritmética. MINUS-KW es la palabra minus,
         ;; MINUS es el signo -
         MINUS-KW ADD MUL QUOTIENT
         ;; Booleanos
         TRUE FALSE EQUAL? LESS? GREATER?
         ;; n-arias
         PLUS TIMES
         ;; Listas
         EMPTYLIST CONS CAR CDR NULL?
         ;; Tarea 1
         NOT AND OR XOR COND ARROW END LIST UNPACK
         ;; Procedimientos
         PROC))

(define-lex-abbrevs
  [digit
   (:/ #\0 #\9)]
  [alpha
   (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [alnum-esp
   (:or alpha digit "_" "-" "?")]
  [number
   (:: (:? "-") (:+ digit))]
  [identifier
   (:: alpha (:* alnum-esp))])

;; Gana el lexema más largo, y a igual longitud la regla que está más arriba.
;; Por eso las palabras reservadas van antes que identifier.
(define next-token
  (lexer-src-pos
   [number
    (token-NUM (string->number lexeme))]

   ["zero?"
    (token-ZERO?)]
   ["if"
    (token-IF)]
   ["then"
    (token-THEN)]
   ["else"
    (token-ELSE)]
   ["let"
    (token-LET)]
   ["in"
    (token-IN)]

   ;; Aritmética
   ["minus"
    (token-MINUS-KW)]
   ["add"
    (token-ADD)]
   ["mul"
    (token-MUL)]
   ["quotient"
    (token-QUOTIENT)]

   ;; Booleanos
   ["true"
    (token-TRUE)]
   ["false"
    (token-FALSE)]
   ["equal?"
    (token-EQUAL?)]
   ["less?"
    (token-LESS?)]
   ["greater?"
    (token-GREATER?)]

   ;; Listas
   ["emptylist"
    (token-EMPTYLIST)]
   ["cons"
    (token-CONS)]
   ["car"
    (token-CAR)]
   ["cdr"
    (token-CDR)]
   ["null?"
    (token-NULL?)]

   ;; Tarea 1
   ["not"
    (token-NOT)]
   ["and"
    (token-AND)]
   ["or"
    (token-OR)]
   ["xor"
    (token-XOR)]
   ["cond"
    (token-COND)]
   ["end"
    (token-END)]
   ["list"
    (token-LIST)]
   ["unpack"
    (token-UNPACK)]

   ;; Procedimientos
   ["proc"
    (token-PROC)]

   [identifier
    (token-ID (string->symbol lexeme))]

   ["-"
    (token-MINUS)]
   ["="
    (token-EQ)]
   ["==>"
    (token-ARROW)]
   ["+"
    (token-PLUS)]
   ["*"
    (token-TIMES)]
   ["("
    (token-LPAREN)]
   [")"
    (token-RPAREN)]
   [","
    (token-COMMA)]

   [whitespace
    (return-without-pos (next-token input-port))]
   [(eof)
    (token-EOF)]
   [any-char
    (raise-read-error-at
     (format "lexer: carácter inesperado: ~a" lexeme)
     input-port start-pos end-pos)]))

;; Error de lectura con la ubicación del texto, para que DrRacket la resalte.
;; La fuente es el nombre del puerto. Con #:eof? el error es exn:fail:read:eof,
;; que dice que el texto se acabó antes de completar una expresión.
(define (raise-read-error-at msg port start end #:eof? [eof? #f])
  ((if eof? raise-read-eof-error raise-read-error)
   msg
   (object-name port)
   (position-line start)
   (position-col start)
   (position-offset start)
   (- (position-offset end) (position-offset start))))

;; El token? de parser-tools da #f para los tokens sin valor, que son
;; símbolos, y #t para un token con valor de cualquier lenguaje.
(define token-names
  '(NUM ID MINUS ZERO? IF THEN ELSE LET EQ IN LPAREN RPAREN COMMA EOF
    MINUS-KW ADD MUL QUOTIENT
    TRUE FALSE EQUAL? LESS? GREATER?
    PLUS TIMES
    EMPTYLIST CONS CAR CDR NULL?
    NOT AND OR XOR COND ARROW END LIST UNPACK
    PROC))

(define empty-token-texts
  (hash 'MINUS "-" 'ZERO? "zero?" 'IF "if" 'THEN "then" 'ELSE "else"
        'LET "let" 'EQ "=" 'IN "in" 'LPAREN "(" 'RPAREN ")" 'COMMA ","
        'EOF "el fin del texto"
        'MINUS-KW "minus" 'ADD "add" 'MUL "mul" 'QUOTIENT "quotient"
        'TRUE "true" 'FALSE "false" 'EQUAL? "equal?" 'LESS? "less?"
        'GREATER? "greater?"
        'PLUS "+" 'TIMES "*"
        'EMPTYLIST "emptylist" 'CONS "cons" 'CAR "car" 'CDR "cdr"
        'NULL? "null?"
        'NOT "not" 'AND "and" 'OR "or" 'XOR "xor" 'COND "cond" 'ARROW "==>"
        'END "end" 'LIST "list" 'UNPACK "unpack"
        'PROC "proc"))

(define (token->text tok)
  (case (token-name tok)
    [(NUM)
     (number->string (token-value tok))]
    [(ID)
     (symbol->string (token-value tok))]
    [else
     (hash-ref empty-token-texts (token-name tok))]))

(define (proc-token? obj)
  (and (or (token? obj) (symbol? obj))
       (memq (token-name obj) token-names)
       #t))

(define (tokenize str)
  (define port (open-input-string str))
  (port-count-lines! port)
  (define (loop)
    (define tok (position-token-token (next-token port)))
    (if (eq? (token-name tok) 'EOF)
        (list tok)
        (cons tok (loop))))
  (loop))
