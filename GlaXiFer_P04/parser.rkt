#lang plai

(require "grammars.rkt")

;; Analizador sintáctico para WAE.
;; Dada una s-expression, construye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> FWBAE
(define (parse sexp)
   (error 'parse "Función no implementada."))

;; Función que elimina el azúcar sintáctica de las expresiones de FWBAE, es decir las convierte a 
;; expresiones de FBAE.
;; desugar: FWBAE -> FBAE
(define (desugar expr)
   (error 'desugar "Función no implementada."))
