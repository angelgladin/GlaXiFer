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
    [(? symbol?) (case sexp ; Símbolo
                   ['true (boolS #t)] ; Booleano verdadero
                   ['false (boolS #f)] ; Booleano false
                   [else (idS sexp)])] ; Identificador
    [(? number?) (numS sexp)] ; Número
    [(list 'with bindings body) ; With
     (withS (aux-parse-bindings bindings) (parse body))]
    [(list 'with* bindings body) ; With*
     (withS* (aux-parse-bindings bindings) (parse body))]
    [(list 'fun (cons x xs) body) ; Función
     (funS (cons x xs) (parse body))]
    [(cons (list 'fun fun-params fun-body) params) ; Aplicación de función
     (appS (funS (cadr fun-params) (parse fun-body)) (aux-parse-params params))]
    [(list 'if cond-expr then-expr else-expr) ; If
     (ifS (parse cond-expr) (parse then-expr) (parse else-expr))]
    [(cons 'cond conditions)
     (condS (aux-parse-conds conditions))]
    [(cons x params) ; Operación
     (opS (elige x) (aux-parse-params params))]))

(define (aux-parse-params params)
  (foldr (λ (v l) (cons (parse v) l)) '() params))

(define (aux-parse-bindings bindings)
  (foldr (λ (v l) (cons (binding (first v) (parse (second v))) l)) '() bindings))

(define (aux-parse-conds conditions)
  (let ([conditions-map (λ (c) (match c
                                 [(list 'else else-expr) (else-cond (parse else-expr))]
                                 [(list expr then-expr) (condition (parse expr) (parse then-expr))]))])
    (foldr (λ (v l) (cons (conditions-map v) l)) '() conditions)))

;; Función que elimina el azúcar sintáctica de las expresiones de CFWBAE/L, es decir las convierte a 
;; expresiones de CFBAE/L.
;; desugar: CFWBAE/L -> CFBAE/L
(define (desugar expr)
   (error 'desugar "Función no implementada."))
