#lang plai

(require "grammars.rkt")

;; Analizador sintáctico para RCFWBAEL/L.
;; Dada una s-expression, costruye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> RCFWBAEL/L.
(define (parse sexp)
	(error 'parse "Función no implementada."))

;; Función que elimina el azúcar sintáctica de las expresiones de RCFWBAEL/L, es decir las convierte a 
;; expresiones de RCFBAEL/L.
;; desugar: RCFWBAEL/L -> RCFBAEL/L
(define (desugar expr)
   (error 'desugar "Función no implementada."))
