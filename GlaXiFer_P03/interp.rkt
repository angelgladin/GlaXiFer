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
    [(id v) (if (symbol=? sub-id v)
                val
                expr)]
    [(op f (cons x xs)) (op f (foldr (λ (v l) (cons (subst v sub-id val) l)) '() (cons x xs)))]
    [(with (cons x xs) bound-body)
         (with (foldr (λ (v l) (cons (binding (binding-name v) (subst (binding-value v) sub-id val) ) l)) '() (cons x xs))
               (if(symbol=? (binding-name x) sub-id)
                     bound-body
                     (subst bound-body sub-id val)))]
    [(with* (cons x xs) bound-body)
         (with* (foldr (λ (v l) (cons (binding (binding-name v) (subst (binding-value v) sub-id val) ) l)) '() (cons x xs))
               (if(symbol=? (binding-name x) sub-id)
                     bound-body
                     (subst bound-body sub-id val)))]
    ))
