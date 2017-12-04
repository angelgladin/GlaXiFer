#lang plai

(require "grammars.rkt")

;; Analizador sintáctico para ERCFWBAEL/L.
;; Dada una s-expression, costruye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> ERCFWBAEL/L.
(define (parse sexp)
	(error 'parse "Función no implementada."))

;; Función que elimina el azúcar sintáctica de las expresiones de ERCFWBAEL/L, es decir las convierte a 
;; expresiones de ERCFBAEL/L.
;; desugar: ERCFWBAEL/L -> ERCFBAEL/L
(define (desugar expr)
   (error 'desugar "Función no implementada."))
