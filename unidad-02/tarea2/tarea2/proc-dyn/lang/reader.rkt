#lang racket/base

;; Lector de #lang proc-dyn. Revisa la sintaxis al leer el archivo, para que un
;; error de lexer o de parser señale su lugar en el archivo, y arma un módulo
;; que pasa el texto a run y escribe el valor en la sintaxis del lenguaje: 4,
;; true, list(1, 2). En la ventana de interacciones queda disponible run.

(require racket/port
         "../parser.rkt")

(provide (rename-out [proc-read read]
                     [proc-read-syntax read-syntax]))

(define (proc-read-syntax fuente entrada)
  (define-values (linea columna posicion) (port-next-location entrada))
  (define texto (port->string entrada))
  ;; Un puerto nuevo sobre el mismo texto, con el nombre del archivo y la
  ;; ubicación en que empieza el programa, después de la línea #lang
  (define copia (open-input-string texto fuente))
  (port-count-lines! copia)
  (when (and linea columna posicion)
    (set-port-next-location! copia linea columna posicion))
  (parse-port copia)
  (datum->syntax
   #f
   `(module programa racket/base
      (require proc-dyn/main proc-dyn/vals)
      (displayln (expval->string (run ,texto))))))

(define (proc-read entrada)
  (syntax->datum (proc-read-syntax #f entrada)))
