#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser.
;; interp: WAE -> number
(define (interp exp)
   (match exp
    [(num n) n]
    [(id v) (error 'interp "Identificador libre")]
    [(op f (cons x xs)) (if (empty? (cdr xs))
                               (f (interp x) (interp (car xs)))
                               (f (interp x) (interp (op f xs))))]
    [(with l bound-body)
         (interp (foldr (λ (l e) (subst bound-body (binding-name l) (num (interp (binding-value l) )))) '() l ))]
    [(with* l bound-body)
         (interp (foldr (λ (l) (subst bound-body (binding-name l) (num (interp (list (binding-value l))))))
                        '()
                        (interp l)))]
    [(cons x xs) (list (subst xs (binding-name x) (binding-value x)) (interp (cons (car xs) (cdr xs))))]
     ))

;(interp (with* (list (binding 'a (num 0)) (binding 'b (num 1))) (op + (list (id 'a) (id 'b)))))

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
