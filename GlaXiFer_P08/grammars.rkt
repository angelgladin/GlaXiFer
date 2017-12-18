#lang plai

;; TDA para representar los árboles de sintáxis abtracta del
;; lenguaje BERCFWBAEL/L. Este TDA es una versión con azúcar sintáctica .
(define-type BERCFWBAEL/L
	[idS (i symbol?)]
	[numS (n number?)]
	[boolS (b boolean?)]
	[listS (elems (listof BERCFWBAEL/L?))]
	[opS (f procedure?) (args (listof BERCFWBAEL/L?))]
	[ifS (expr BERCFWBAEL/L?) (then-expr BERCFWBAEL/L?) (else-expr BERCFWBAEL/L?)]
	[condS (cases (listof Condition?))]
	[withS (bindings (listof bindingS?)) (body BERCFWBAEL/L?)]
	[withS* (bindings (listof bindingS?)) (body BERCFWBAEL/L?)]
	[recS (bindings (listof bindingS?)) (body BERCFWBAEL/L?)]
	[funS (params (listof symbol?)) (body BERCFWBAEL/L?)]
	[appS (fun-expr BERCFWBAEL/L?) (args (listof BERCFWBAEL/L?))]
	[throwsS (exception-id symbol?)]
	[try/catchS (bindings (listof bindingS?)) (body BERCFWBAEL/L?)]
	[newboxS (contents BERCFWBAEL/L?)]
	[openboxS (box BERCFWBAEL/L?)]
	[setboxS (box BERCFWBAEL/L?) (contents BERCFWBAEL/L?)]
	[seqnS (actions (listof BERCFWBAEL/L?))])

;; TDA para representar los árboles de sintaxis abstracta del
;; lenguaje BERCFBAEL/L. Este TDA es una versión sin azúcar sintáctica.
(define-type BERCFBAEL/L
	[id (i symbol?)]
	[num (n number?)]
	[bool (b boolean?)]
	[lisT (elems (listof BERCFBAEL/L?))]
	[op (f procedure?) (args (listof BERCFBAEL/L?))]
	[iF (expr BERCFBAEL/L?) (then-expr BERCFBAEL/L?) (else-expr BERCFBAEL/L?)]
	[fun (params (listof symbol?)) (body BERCFBAEL/L?)]
	[rec (bindings (listof Binding?)) (body BERCFBAEL/L?)]
	[app (fun-expr BERCFBAEL/L?) (args (listof BERCFBAEL/L?))]
	[throws (exception-id symbol?)]
	[try/catch (bindings (listof Binding?)) (body BERCFBAEL/L?)]
	[newbox (content BERCFBAEL/L?)]
	[openbox (box BERCFBAEL/L?)]
	[setbox (box BERCFBAEL/L?) (content BERCFBAEL/L?)]
	[seqn (actions (listof BERCFBAEL/L?))])

;; TDA para representar los resultados devueltos por el intérprete .
(define-type BERCFBAEL/L-Value
	[numV (n number?)]
	[boolV (b boolean?)]
	[closureV (params (listof symbol?)) (body BERCFBAEL/L?) (env Env?)]
	[exprV (expr BERCFBAEL/L?) (env Env?)]
	[listV (elems (listof BERCFBAEL/L-Value?))]
	[exceptionV (exception-id symbol?) (continuation continuation?)]
	[boxV (location number?)])

;; TDA para asociar identificadores con valores con azúcar sintáctica .
(define-type BindingS
	[bindingS (name symbol?) (value BERCFWBAEL/L?)])

;; TDA para asociar identificadores con valores sin azúcar sintáctica .
(define-type Binding
	[binding (name symbol?) (value BERCFBAEL/L?)])

;; TDA para representar condiciones .
(define-type Condition
	[condition (expr BERCFWBAEL/L?) (then-expr BERCFWBAEL/L?)]
	[else-cond (else-expr BERCFWBAEL/L?)])

;; TDA para representar los ambientes de evaluación .
(define-type Env
	[mtSub]
	[aSub (name symbol?) (location number?) (env Env?)])

;; TDA para representar el store
(define-type Store
	[mtSto]
	[aSto (index number?) (value BERCFBAEL/L-Value?) (sto Store?)])

(define-type Value*Store
   [v*s (value BERCFBAEL/L-Value?) (store Store?)])

(define-type List-Value*Store
  [lv*s (value (listof BERCFBAEL/L-Value?)) (store Store?)])

(define-type Env*Store
  [e*s (env Env?) (store Store?)])