#lang plai

(require "grammars.rkt")


;; Función auxiliar que hace un mapeo entre los operadores en
;; sintaxis concreta y los operadores de Racket . Esto con el fin de
;; apli car la operación más adelante .
;; elige : symbol - > procedure
(define (elige sexp)
  (match sexp
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['% mmodulo]
    ['min min]
    ['max max]
    ['pow mexpt]
    ['zero? zero?]
    ['not not]
    ['and and-aux]
    ['or or-aux]
    ['< <]
    ['> >]
    ['<= <=]
    ['>= >=]
    ['= =]
    ['/= not-eq]
    ['head car]
    ['tail cdr]
    ['empty? empty?]))

;; Función auxiliar auxiliar que hace que hace un `or` que recibe n argumentos
;; se hizo porque la función `or` es un macro y no una función.
(define (or-aux . bools)
  (ormap (λ (x) x) bools))

;; Función auxiliar que hace que hace un `and` que recibe n argumentos
;; se hizo porque la función `and` es un macro y no una función.
(define (and-aux . bools)
  (andmap (λ (x) x) bools))

;; Función auxiliar que hace que hace un `/=` que recibe n argumentos
;; se hizo porque la función `/=` es un macro y no una función.
(define (not-eq . params)
  (not (apply = params)))

;; Potencia multiparamétrica.
;; Función que recibe n elementos, a diferencia `expt` que solo recibe 2 elementos.
(define (mexpt . params)
  (opMultiparametrica expt params))

;; Módulo multiparamétrico.
;; Función que recibe n elementos, a diferencia de `modulo` que solo recibe 2 elementos.
(define (mmodulo . params)
  (opMultiparametrica modulo params))

;; Función auxiliar que dada una función que solo recibe dos parámetros la convierte
;; en una función asociativa que recibe n parámetros y siempre va asociando
;; el resultado con el siguiente elemento.
;; opMultiparametrica: procedure -> list number -> number
(define (opMultiparametrica f params)
  (letrec ([aux (λ (n l)
                  (match l
                    ['() n]
                    [(cons x xs) (aux (f n x) xs)]))])
    (aux (f (first params) (second params)) (cdr (cdr params)))))


;; Analizador sintáctico para RCFWBAEL/L.
;; Dada una s-expression, costruye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> RCFWBAEL/L.
(define (parse sexp)
  (match sexp
    [(? symbol?) (case sexp ; Símbolo
                   ['true (boolS #t)] ; Booleano verdadero
                   ['false (boolS #f)] ; Booleano false
                   [else (idS sexp)])] ; Identificador
    [(cons 'list elems) (listS (map parse elems))] ; Lista
    [(? number?) (numS sexp)] ; Número
    [(list 'with bindings body) ; With
     (withS (aux-parse-bindings bindings) (parse body))]
    [(list 'with* bindings body) ; With*
     (withS* (aux-parse-bindings bindings) (parse body))]
    [(list 'rec bindings body) ; Rec
     (recS (aux-parse-bindings bindings) (parse body))]
    [(list 'fun (cons x xs) body) ; Función
     (funS (cons x xs) (parse body))]
    [(list (list 'fun fun-params fun-body) params) ; Aplicación de función
     (appS (funS fun-params (parse fun-body)) (map parse params))]
    [(list 'if cond-expr then-expr else-expr) ; If
     (ifS (parse cond-expr) (parse then-expr) (parse else-expr))]
    [(cons 'cond conditions) ; Cond
     (condS (aux-parse-conds conditions))]
    [(cons x xs) ; Aquí hay dos casos; Una operación o aplicación de función
     (case x
       ; Checamos si es una operación del lenguaje.
       [(+ - * / % min max pow zero? not and or < > <=  >= = /= head tail empty?)
        (opS (elige x) (map parse xs))]
       ; Aplicación de función
       [else (appS (parse x) (map parse xs))])]))

;; Función auxiliar que hace un crea una lista de `bindings`.
;; aux-parse-bindings list list symbol -> list BindingS
(define (aux-parse-bindings bindings)
  (map (λ (v) (bindingS (first v) (parse (second v)))) bindings))

;; Función auxiliar que hace maneja cada posible valor que puede tener un condition
;; aux-parse-conds list symbol -> list Condition
(define (aux-parse-conds conditions)
  (let ([conditions-map (λ (c) (match c
                                 [(list 'else else-expr) (else-cond (parse else-expr))]
                                 [(list expr then-expr) (condition (parse expr) (parse then-expr))]))])
    (map (λ (v) (conditions-map v)) conditions)))


;; Función que elimina el azúcar sintáctica de las expresiones de RCFWBAEL/L, es decir las convierte a 
;; expresiones de RCFBAEL/L.
;; desugar: RCFWBAEL/L -> RCFBAEL/L
(define (desugar expr)
  (match expr
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(boolS b) (bool b)]
    [(opS f args) (op f (map desugar args))]
    [(listS elem) (desugar elem)]
    [(ifS expr then-expr else-expr) (iF (desugar expr) (desugar then-expr) (desugar else-expr))]
    [(condS (cons x xs)) (match x
                           [(condition expr then-expr) (desugar (ifS expr then-expr (condS xs)))]
                           [(else-cond else-expr)  (desugar else-expr)])]
    [(withS bindings body) (app (fun (map bindingS-name bindings) (desugar body))
                                (map (λ (v) (desugar (bindingS-value v))) bindings))]
    [(withS* (cons x xs) body) (desugar (withS (list x) (if (empty? xs) body (withS* xs body))))]
    [(funS params body) (fun params (desugar body))]
    [(recS bindings body) (app (fun (map bindingS-name bindings) (desugar body))
                               (map (λ (v) (desugar (bindingS-value v))) bindings))]
    [(appS fun-expr args) (app (desugar fun-expr) (map desugar args))]))
