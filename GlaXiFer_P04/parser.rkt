#lang plai

(require "grammars.rkt")

;; [ Auxiliar ]. Función que hace un mapeo entre los operadores en
;; sintaxis concreta y los operadores de Racket . Esto con el fin de
;; apli car la operación más adelante .
;; elige : symbol - > procedure
(define (elige sexp)
  (match sexp
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['% modulo]
    ['min min]
    ['max max]
    ['pow mexpt]
    ['not not]
    ['and and-aux]
    ['or or-aux]
    ['< <]
    ['> >]
    ['<= <=]
    ['>= >=]
    ['= =]
    ['/= not-eq]))

(define (or-aux . bools)
  (ormap (λ (x) x) bools))

(define (and-aux . bools)
  (andmap (λ (x) x) bools))

(define (not-eq . params)
  (if (= (set-count (list->set params)) 1) #t #f))

;; Potencia multiparamétrica.
;; Función que recibe n elementos, a diferencia expt que solo recibe 2 elementos.
(define mexpt
  (λ nums (foldr expt (car nums) (cdr nums))))

;; Analizador sintáctico para WAE.
;; Dada una s-expression, construye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> FWBAE
(define (parse sexp)
   (match sexp
     ['true (boolS #t)]
     ['false (boolS #f)]
     [(? symbol?) (idS sexp)]
     [(? number?) (numS sexp)]
     [(list 'with (cons x xs) body)
     (withS (foldr (λ (v l) (cons (binding (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
     [(list 'with* (cons x xs) body)
     (withS* (foldr (λ (v l) (cons (binding (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
     [(list 'fun (cons x xs) body)
      (funS (cons x xs) (parse body))]
     [(list 'app fun-expr args)
      (appS (parse fun-expr) (foldr (λ (v l) (cons (parse v) l)) '()args))]
     [(cons x xs)
      (opS (elige x) (foldr (λ (v l) (cons (parse v) l)) '() xs))]))

;; Función que elimina el azúcar sintáctica de las expresiones de FWBAE, es decir las convierte a 
;; expresiones de FBAE.
;; desugar: FWBAE -> FBAE
(define (desugar expr)
   (error 'desugar "Función no implementada."))
