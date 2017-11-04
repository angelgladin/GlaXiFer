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
;; se hizo porque la función `or` es un macro y no una función.
(define (or-aux . bools)
  (ormap (λ (x) x) bools))

;; [ Auxiliar ]. Función que hace que hace un `and` que recibe n argumentos
;; se hizo porque la función `and` es un macro y no una función.
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
    [(list (list 'fun fun-params fun-body) params) ; Aplicación de función
     (appS (funS fun-params (parse fun-body)) (aux-parse-params params))]
    [(list 'if cond-expr then-expr else-expr) ; If
     (ifS (parse cond-expr) (parse then-expr) (parse else-expr))]
    [(cons 'cond conditions) ; Cond
     (condS (aux-parse-conds conditions))]
    [(cons x params) ; Operación
     (opS (elige x) (aux-parse-params params))]))

;; Función auxiliar que hace un `parse` a cada elemeto de una lista. Se usa cuando
;; una tenemos una lista que son varios símbolos.
;; aux-parse-params: list symbol -> list CFWBAE/L
(define (aux-parse-params params)
  (foldr (λ (v l) (cons (parse v) l)) '() params))

;; Función auxiliar que hace un crea una lista de `bindings`.
;; aux-parse-bindings list list symbol -> list Binding
(define (aux-parse-bindings bindings)
  (foldr (λ (v l) (cons (binding (first v) (parse (second v))) l)) '() bindings))

;; Función auxiliar que hace maneja cada posible valor que puede tener un condition
;; aux-parse-conds list symbol -> list Condition
(define (aux-parse-conds conditions)
  (let ([conditions-map (λ (c) (match c
                                 [(list 'else else-expr) (else-cond (parse else-expr))]
                                 [(list expr then-expr) (condition (parse expr) (parse then-expr))]))])
    (foldr (λ (v l) (cons (conditions-map v) l)) '() conditions)))

;; Función que elimina el azúcar sintáctica de las expresiones de CFWBAE/L, es decir las convierte a 
;; expresiones de CFBAE/L.
;; desugar: CFWBAE/L -> CFBAE/L
(define (desugar expr)
  (match expr
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(boolS b) (bool b)]
    [(opS f args) (op f (desugar-params args))]
    [(ifS expr then-expr else-expr) (iF (desugar expr) (desugar then-expr) (desugar else-expr))]
    [(condS (cons x xs)) (match x
                           [(condition expr then-expr) (desugar (ifS expr then-expr (condS xs)))]
                           [(else-cond else-expr)  (desugar else-expr)])]
    [(withS bindings body) (app (fun (with-names bindings) (desugar body)) (with-values bindings))]
    [(withS* (cons x xs) body) (desugar (withS (list x) (if (empty? xs) body (withS* xs body))))]
    [(funS params body) (fun params (desugar body))]
    [(appS fun-expr args) (app (desugar fun-expr) (desugar args))]))

;; Función auxiliar que hace un `desugar` a cada elemeto de la lista.
;; aux-parse-params: list CFWBAE/L -> list CFBAE/L
(define (desugar-params params)
  (foldr (λ (v l) (cons (desugar v) l)) '() params))

;; Función auxiliar que da una lista que contiene los nombre de cada Binding. Esta función
;; es usada en la aplicación de función porque solo da el nombre de los parámetros
;; formales de la función.
;; aux-parse-params: list Binding -> list symbol
(define (with-names bindings)
  (foldr (λ (v l) (cons (binding-name v) l)) '() bindings))

;; Función auxiliar que da una lista que contiene los valores de cada Binding. Esta función
;; es usada en la aplicación de función porque da el valor de cada parámetro asociado.
;; aux-parse-params: list Binding -> list CFBAE/L?
(define (with-values bindings)
  (foldr (λ (v l) (cons (desugar (binding-value v)) l)) '() bindings))
