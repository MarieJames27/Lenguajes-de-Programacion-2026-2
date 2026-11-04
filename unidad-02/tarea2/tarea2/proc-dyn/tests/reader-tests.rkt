#lang racket

;; Pruebas de lang/reader.rkt: el lector de #lang proc-dyn

(require rackunit
         "../lang/reader.rkt")

;; El lector recibe el puerto ya posicionado después de la línea #lang
(define (port-after-lang str)
  (define port (open-input-string str 'prog.rkt))
  (port-count-lines! port)
  (set-port-next-location! port 2 0 12)
  port)

(check-equal? (syntax->datum (read-syntax 'prog.rkt (port-after-lang "-(3, 1)\n")))
              '(module programa racket/base
                 (require proc-dyn/main proc-dyn/vals)
                 (displayln (expval->string (run "-(3, 1)\n"))))
              "un programa válido se vuelve un módulo que llama a run")

(check-equal? (with-handlers ([exn:fail:read?
                               (lambda (e)
                                 (define loc (first (exn:fail:read-srclocs e)))
                                 (list (srcloc-source loc) (srcloc-line loc)
                                       (srcloc-column loc)))])
                (read-syntax 'prog.rkt (port-after-lang "let x = 5\nin let y =\n   in y\n")))
              '(prog.rkt 4 3)
              "el error de sintaxis señala la línea del archivo, contando la de #lang")

(check-exn #rx"^prog.rkt:3:8: lexer: carácter inesperado: @"
           (lambda () (read-syntax 'prog.rkt (port-after-lang "let x = 5\nin -(x, @)\n")))
           "el error del lexer también, con el nombre del archivo")
