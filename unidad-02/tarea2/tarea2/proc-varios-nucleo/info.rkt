#lang info

;; Todas las versiones de PROC instalan la colección proc: solo una a la vez
(define collection "proc")
(define pkg-desc "PROC con procedimientos de varios parámetros, en el núcleo")
(define deps '("base" "parser-tools-lib" "rackunit-lib"))
