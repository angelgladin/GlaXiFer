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

;; [ Auxiliar ]. Función que hace que hace un `or` que recibe n argumentos
;; se hizo porque la función `or` es un macro y un una función.
(define (or-aux . bools)
  (ormap (λ (x) x) bools))

;; [ Auxiliar ]. Función que hace que hace un `and` que recibe n argumentos
;; se hizo porque la función `and` es un macro y un una función.
(define (and-aux . bools)
  (andmap (λ (x) x) bools))

;; [ Auxiliar ]. Función que hace que hace un `/=` que recibe n argumentos
;; se hizo porque la función `/=` es un macro y no una función.
(define (not-eq . params)
  (not (apply = params)))

;; Potencia multiparamétrica.
;; Función que recibe n elementos, a diferencia expt que solo recibe 2 elementos.
(define mexpt
  (λ nums (foldr expt (car nums) (cdr nums))))

;; Analizador sintáctico para CFWBAE/L.
;; Dada una s-expression, construye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> CFWBAE/L
(define (parse sexp)
  (match sexp
    [(? symbol?) (case sexp
                   ['true (boolS #t)]
                   ['false (boolS #f)]
                   [else (idS sexp)])]
    [(? number?) (numS sexp)]
    [(list 'with (cons x xs) body)
     (withS (foldr (λ (v l) (cons (binding (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
    [(list 'with* (cons x xs) body)
     (withS* (foldr (λ (v l) (cons (binding (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
    [(list 'fun (cons x xs) body)
     (funS (cons x xs) (parse body))]
    [(list 'app fun-expr args)
     (appS (parse fun-expr) (foldr (λ (v l) (cons (parse v) l)) '() args))]
    [(list 'if cond-expr then-expr else-expr)
     (ifS (parse cond-expr) (parse then-expr) (parse else-expr))]
    [(cons x xs)
     (opS (elige x) (foldr (λ (v l) (cons (parse v) l)) '() xs))]))

;; Función que elimina el azúcar sintáctica de las expresiones de CFWBAE/L, es decir las convierte a 
;; expresiones de CFBAE/L.
;; desugar: CFWBAE/L -> CFBAE/L
(define (desugar expr)
   (error 'desugar "Función no implementada."))
