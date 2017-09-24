#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser.
;; interp: WAE -> number
(define (interp exp)
   (match exp
    [(num n) n]
    [(id v) (error 'interp "Identificador libre")]
    [(op f (cons x xs)) (if (empty? (cdr  xs))
                            (f (interp x) (interp (car xs)))
                            (let ([r  (f (interp x) (interp (car xs)))])
                                             (interp (op f (cons (num r) (cdr xs)) ))))]
    [(with l bound-body)
        (interp (foldr (λ (l e) (subst e (binding-name l) (num (interp (binding-value l))) )) bound-body l ))]
    [(with* l bound-body)
         (interp (with (interp l) bound-body))]

    [(cons x xs) (cond
                   [(empty? xs) (cons x xs)]
                   [(empty? (cdr xs))
                         (cons (binding (binding-name (car xs)) (subst (binding-value (car xs)) (binding-name x) (binding-value x))) empty)]
                   [(cons x (interp (cdr (cons x (map (λ (v) (binding (binding-name v) (subst (binding-value v) (binding-name x) (binding-value x)))) xs)))))]
                 )]

    ))


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
                     (subst bound-body sub-id val)))]))
