#lang racket

;; Punto de entrada: run y el REPL

(require "env.rkt"
         "vals.rkt"
         "parser.rkt"
         "desugar.rkt"
         "interp.rkt")

(provide
 (contract-out
  [run (->* (string?) (any/c) expval?)]
  [repl (-> void?)]))

(define (run str [source 'string])
  (value-of (desugar (parse str source)) (empty-env)))

;; Una línea vacía o el fin de la entrada terminan el REPL. Ctrl-C cancela la
;; expresión que se está escribiendo y vuelve al prompt.
(define (repl)
  (define continue?
    (with-handlers ([exn:break?
                     (λ (e) (newline) #t)])
      (read-eval-print)))
  (when continue?
    (repl)))

;; Devuelve #f si el REPL tiene que terminar.
(define (read-eval-print)
  (display "==> ")
  (define line (read-line))
  (cond
    [(or (eof-object? line) (string=? (string-trim line) ""))
     #f]
    [else
     (evaluate line)
     #t]))

;; Si el texto está incompleto, pide otra línea y vuelve a intentar con el
;; texto completo. La línea se pide fuera del manejador de with-handlers,
;; porque ahí Ctrl-C está desactivado.
(define (evaluate text)
  (define incomplete
    (with-handlers ([exn:fail:read:eof?
                     (λ (e) e)]
                    [exn:fail?
                     (λ (e) (displayln (exn-message e)) #f)])
      (displayln (expval->string (run text 'repl)))
      #f))
  (when incomplete
    (continue text incomplete)))

(define (continue text incomplete)
  (display "... ")
  (define line (read-line))
  (if (eof-object? line)
      (displayln (exn-message incomplete))
      (evaluate (string-append text "\n" line))))

(module+ main
  (repl))

