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
  (match expr
    [(num n) expr]
    [(op f (list l r)) (op f (list (subst l sub-id val) (subst r sub-id val)))]
    [(with (list bound-id named-expr) bound-body)
         (if(symbol=? bound-id sub-id)
            expr
            (with bound-id named-expr
                  (subst bound-body sub-id val)))]
    [(id v) (if(symbol=? v sub-id)
               val
               expr)]))
