#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser. El
;; intérprete requiere un ambiente de evaluación en esta versión para buscar el valor de los 
;; identificadores.
;; interp: RCFBAEL/L Env -> RCFBAEL/L-Value
(define (interp expr env)
	(error 'interp "Función no implementada."))
