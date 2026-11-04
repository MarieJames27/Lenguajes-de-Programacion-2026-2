#lang racket

;; Pruebas de lexer.rkt: del texto a los tokens

(require rackunit
         "../lexer.rkt")

(define (names str)
  (map token-name (tokenize str)))

(define (first-value str)
  (token-value (first (tokenize str))))

;; Los tokens base, con dos cadenas

(check-equal? (names "let x = 7 in -(x, 2)")
              '(LET ID EQ NUM IN MINUS LPAREN ID COMMA NUM RPAREN EOF)
              "diez tokens distintos en un let con resta")

(check-equal? (names "if zero?(x) then 1 else 2")
              '(IF ZERO? LPAREN ID RPAREN THEN NUM ELSE NUM EOF)
              "if, then, else y zero?")

;; Los valores que cargan los tokens

(check-equal? (first-value "7") 7
              "el lexema ya viene convertido a entero, no es la cadena")

(check-equal? (first-value "x") 'x
              "el nombre viene como símbolo")

(check-equal? (first-value "let") #f
              "un token sin valor no carga nada")

;; Espacios y texto vacío

(check-equal? (tokenize "  7\n") (tokenize "7")
              "los espacios no producen tokens")

(check-equal? (names "") '(EOF)
              "el texto vacío es solo EOF")

;; Las dos reglas de desempate

(check-equal? (names "-5") '(NUM EOF)
              "[1] el guion entra al número: un solo token")

(check-equal? (first-value "-5") -5
              "[1] y el valor es el entero negativo")

(check-equal? (names "-(") '(MINUS LPAREN EOF)
              "sin dígito después, el guion es MINUS")

(check-equal? (names "--5") '(MINUS NUM EOF)
              "solo un guion cabe en el número")

(check-equal? (first-value "x-1") 'x-1
              "[1] x-1 es un identificador, no una resta")

(check-equal? (names "zero?") '(ZERO? EOF)
              "[2] a igual longitud gana la palabra reservada, que está arriba")

(check-equal? (first-value "zero?y") 'zero?y
              "[1] más largo le gana a la palabra reservada")

;; Errores, y lo que no es error

(check-exn #rx"carácter inesperado: @"
           (lambda () (tokenize "3 @ 4"))
           "el error propio del lexer dice el carácter")

(check-exn #rx"carácter inesperado"
           (lambda () (tokenize "3.5"))
           "PROC no tiene flotantes: falla en el punto")

(check-equal? (names "zero?(3 3") '(ZERO? LPAREN NUM NUM EOF)
              "pasa sin queja: el orden de los tokens es asunto del parser")

;; Posiciones: cada token sabe dónde empieza y dónde termina

(define (positions str)
  (define port (open-input-string str))
  (port-count-lines! port)
  (define (loop)
    (define pt (next-token port))
    (define start (position-token-start-pos pt))
    (if (eq? (token-name (position-token-token pt)) 'EOF)
        '()
        (cons (list (position-line start) (position-col start))
              (loop))))
  (loop))

(check-equal? (positions "let x\n  = 7")
              '((1 0) (1 4) (2 2) (2 4))
              "línea desde 1 y columna desde 0, como Racket")

(check-equal? (let* ([port (open-input-string "  zero?")]
                     [_ (port-count-lines! port)]
                     [pt (next-token port)])
                (list (position-offset (position-token-start-pos pt))
                      (position-offset (position-token-end-pos pt))))
              '(3 8)
              "los espacios no cuentan como token, pero sí como posición")

(check-exn exn:fail:read?
           (lambda () (tokenize "3 @ 4"))
           "el error del lexer es un error de lectura")

(check-equal? (with-handlers ([exn:fail:read?
                               (lambda (e)
                                 (define loc (first (exn:fail:read-srclocs e)))
                                 (list (srcloc-line loc) (srcloc-column loc)
                                       (srcloc-span loc)))])
                (tokenize "let x = 7\nin -(x, @)"))
              '(2 8 1)
              "el error del lexer dice la línea y la columna del carácter")

;; El texto de cada token, para los mensajes de error

(check-true (andmap (lambda (name) (string? (token->text name)))
                    (remove* '(NUM ID) token-names))
            "cada token sin valor tiene su texto")

(check-equal? (map token->text (tokenize "let x = -5 in ==> )"))
              '("let" "x" "=" "-5" "in" "==>" ")" "el fin del texto")
              "un token con valor se muestra con su valor")

;; El predicado

(check-true (andmap proc-token? (tokenize "let x = 7 in x"))
            "todo lo que sale del lexer es un token de PROC")

(check-false (proc-token? 7)
             "un entero no es un token")

;; Aritmética: cuatro palabras reservadas

(check-equal? (names "minus(1) add(1, 2) mul(1, 2) quotient(1, 2)")
              '(MINUS-KW LPAREN NUM RPAREN
                ADD LPAREN NUM COMMA NUM RPAREN
                MUL LPAREN NUM COMMA NUM RPAREN
                QUOTIENT LPAREN NUM COMMA NUM RPAREN EOF)
              "las cuatro palabras de la aritmética")

(check-equal? (names "minus(-3)") '(MINUS-KW LPAREN NUM RPAREN EOF)
              "la palabra minus y el signo - son tokens distintos")

(check-equal? (first-value "minus-one") 'minus-one
              "[1] más largo le gana a la palabra reservada, también aquí")

;; Booleanos: cinco palabras reservadas

(check-equal? (names "true false") '(TRUE FALSE EOF)
              "las dos constantes")

(check-equal? (names "equal?(1, 2) less?(1, 2) greater?(1, 2)")
              '(EQUAL? LPAREN NUM COMMA NUM RPAREN
                LESS? LPAREN NUM COMMA NUM RPAREN
                GREATER? LPAREN NUM COMMA NUM RPAREN EOF)
              "las tres comparaciones")

(check-equal? (first-value "true?") 'true?
              "[1] más largo le gana a la palabra reservada: true? es un nombre")

;; n-arias: los signos + y *

(check-equal? (names "+(1, 2, 3)")
              '(PLUS LPAREN NUM COMMA NUM COMMA NUM RPAREN EOF)
              "el signo + es un token de un carácter")

(check-equal? (names "*(1, 2, 3)")
              '(TIMES LPAREN NUM COMMA NUM COMMA NUM RPAREN EOF)
              "el signo * es un token de un carácter")

;; Listas: cinco palabras reservadas

(check-equal? (names "cons(1, emptylist)")
              '(CONS LPAREN NUM COMMA EMPTYLIST RPAREN EOF)
              "cons y emptylist")

(check-equal? (names "car(x) cdr(x) null?(x)")
              '(CAR LPAREN ID RPAREN CDR LPAREN ID RPAREN NULL? LPAREN ID RPAREN EOF)
              "car, cdr y null?")

(check-equal? (first-value "cons-cell") 'cons-cell
              "[1] más largo le gana a la palabra reservada, también aquí")

;; Tarea 1: ocho palabras reservadas y la flecha

(check-equal? (names "not(x) and(x, y) or(x, y) xor(x, y)")
              '(NOT LPAREN ID RPAREN
                AND LPAREN ID COMMA ID RPAREN
                OR LPAREN ID COMMA ID RPAREN
                XOR LPAREN ID COMMA ID RPAREN EOF)
              "los cuatro booleanos")

(check-equal? (names "cond zero?(x) ==> 1 end")
              '(COND ZERO? LPAREN ID RPAREN ARROW NUM END EOF)
              "cond, la flecha y end")

(check-equal? (names "unpack x y = list(1, 2) in x")
              '(UNPACK ID ID EQ LIST LPAREN NUM COMMA NUM RPAREN IN ID EOF)
              "unpack y list")

(check-equal? (names "= ==> =") '(EQ ARROW EQ EOF)
              "[1] la flecha le gana al signo igual por ser más larga")

(check-equal? (first-value "android") 'android
              "[1] más largo le gana a la palabra reservada: android es un nombre")

;; Procedimientos

(check-equal? (names "proc (x) (f x)")
              '(PROC LPAREN ID RPAREN LPAREN ID ID RPAREN EOF)
              "proc es palabra reservada, y la llamada son paréntesis con dos expresiones")

(check-equal? (first-value "procedure") 'procedure
              "[1] procedure es un identificador: más largo que proc")
