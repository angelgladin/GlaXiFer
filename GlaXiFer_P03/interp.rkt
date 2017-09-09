#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser.
;; interp: WAE -> number
(define (interp exp)
   (error 'interp "Función no implementada."))

;; Función que implementa el algoritmo de sustitución.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sub-id val)
   (error 'subst "Función no implementa."))
