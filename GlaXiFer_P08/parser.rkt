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


;; Analizador sintáctico para BERCFWBAEL/L.
;; Dada una s-expression, costruye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> ERCFWBAEL/L.
(define (parse sexp)
  (match sexp
    [(? symbol?) (case sexp
                   ['true (boolS #t)] ; Booleano verdadero: 'true
                   ['false (boolS #f)] ; Booleano false: 'false
                   [else (idS sexp)])] ; Identificador: 'foo
    ; Si es una Lista: {list 1 2 3}
    [(cons 'list elems) (listS (map parse elems))]
    ; Si es un número: 666
    [(? number?) (numS sexp)]
    ; Si es un with: {with {{a 1}} {+ a a}}
    [(list 'with bindings body)
     (withS (aux-parse-bindingS bindings) (parse body))]
    ; Si es un with*: {with* {{a 2} {b a}} {+ a b}}
    [(list 'with* bindings body)
     (withS* (aux-parse-bindingS bindings) (parse body))]
    ; Si es una función recursiva: {rec {{fac {fun {n}
    ;                                       {if {zero? n} 1
    ;                                           {* n {fac {- n 1}}}}}}
    ;                                   {fac n}}
    [(list 'rec bindings body)
     (recS (aux-parse-bindingS bindings) (parse body))]
    ; Si es una función: {fun {x} {+ x 2}
    [(list 'fun (cons x xs) body)
     (funS (cons x xs) (parse body))]
    ; Aplicación de función: {{fun {a} {+ a 0}} {666}}
    [(list (list 'fun fun-params fun-body) params)
     (appS (funS fun-params (parse fun-body)) (map parse params))]
    ; Si es un condicional if: {if {< 2 3} 4 5} 
    [(list 'if cond-expr then-expr else-expr)
     (ifS (parse cond-expr) (parse then-expr) (parse else-expr))]
    ; Si es un condicional cond: {cond {{< 1 2} 666} {else 0}}
    [(cons 'cond conditions)
     (condS (aux-parse-conds conditions))]
    ; El identificador de un throws: {throws MiExcepcion}
    [(list 'throws exception-id) (throwsS exception-id)]
    ; Try/catch: {try/catch {{MiExcepcion 666}} {+ 0 {throws MiExcepcion}}}
    [(list 'try/catch bindings body)
     (try/catchS (aux-parse-bindingS bindings) (parse body))]
    ; Si es una nueva caja: {newbox 666}
    [(list 'box value) (newboxS (parse value))]
    ; Si se asigna valor a una caja: {setbox caja 666}
    [(list 'setbox box value) (setboxS (parse box) (parse value))]
    ; Si se abre una caja: {openbox caja}
    [(list 'openbox box) (openboxS (parse box))]
    ; Para ejecutar n acciones: {seqn {setbox caja 666} {openbox caja}}
    [(cons 'seqn actions) (seqnS (map parse actions))]
    [(cons x xs) ; Aquí hay dos casos
     (case x
       ; Checamos si es una operación del lenguaje.
       [(+ - * / % min max pow zero? not and or < > <=  >= = /= head tail empty?)
        (opS (elige x) (map parse xs))]
       ; Aplicación de función
       [else (appS (parse x) (map parse xs))])]))


;; Función auxiliar que hace un crea una lista de `bindings`.
;; aux-parse-bindings list list symbol -> list BindingS
(define (aux-parse-bindingS bindings)
  (map (λ (v) (bindingS (first v) (parse (second v)))) bindings))

;; Función auxiliar que hace maneja cada posible valor que puede tener un condition
;; aux-parse-conds list symbol -> list Condition
(define (aux-parse-conds conditions)
  (let ([conditions-map (λ (c) (match c
                                 [(list 'else else-expr) (else-cond (parse else-expr))]
                                 [(list expr then-expr) (condition (parse expr) (parse then-expr))]))])
    (map (λ (v) (conditions-map v)) conditions)))

;; Función que elimina el azúcar sintáctica de las expresiones de ERCFWBAEL/L, es decir las convierte a 
;; expresiones de BERCFBAEL/L.
;; desugar: BERCFWBAEL/L -> BERCFBAEL/L
(define (desugar expr)
  (match expr
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(boolS b) (bool b)]
    [(listS elems) (lisT (map desugar elems))]
    [(opS f args) (op f (map desugar args))]
    [(ifS expr then-expr else-expr) (iF (desugar expr) (desugar then-expr) (desugar else-expr))]
    [(condS (cons x xs)) (match x
                           [(condition expr then-expr) (desugar (ifS expr then-expr (condS xs)))]
                           [(else-cond else-expr)  (desugar else-expr)])]
    [(withS bindings body) (app (fun (map bindingS-name bindings) (desugar body))
                                (map (λ (v) (desugar (bindingS-value v))) bindings))]
    [(withS* (cons x xs) body) (desugar (withS (list x) (if (empty? xs) body (withS* xs body))))]
    [(recS bindings body) (rec (aux-parse-binding bindings) (desugar body))]
    [(funS params body) (fun params (desugar body))]
    [(appS fun-expr args) (app (desugar fun-expr) (map desugar args))]
    [(throwsS exception-id) (throws exception-id)]
    [(try/catchS bindings body) (try/catch (aux-parse-binding bindings) (desugar body))]
    [(newboxS contents) (newbox (desugar contents))]
    [(openboxS box) (openbox (desugar box))]
    [(setboxS box contents) (setboxS (desugar box) (desugar contents))]
    [(seqnS actions) (seqnS (map desugar actions))]))

;; Función auxiliar que hace un crea una lista de `bindings`.
;; aux-parse-bindings list list symbol -> list Binding
(define (aux-parse-binding bindings)
  (map (λ (v) (binding (bindingS-name v) (desugar (bindingS-value v)))) bindings))
