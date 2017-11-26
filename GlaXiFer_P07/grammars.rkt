#lang plai

;; TDA para representar los árboles de sintáxis abtracta del lenguaje ERCFWBAEL/L.
;; Este TDA es una versión con azúcar sintáctica.
(define-type ERCFWBAEL/L
   [idS (i symbol?)]
   [numS (n number?)]
   [boolS (b boolean?)]
   [listS (elems (listof ERCFWBAEL/L?))]
   [opS (f procedure?) (args (listof ERCFWBAEL/L?))]
   [ifS (expr ERCFWBAEL/L?) (then-expr ERCFWBAEL/L?) (else-expr ERCFWBAEL/L?)]
   [condS (cases (listof Condition?))]
   [withS (bindings (listof bindingS?)) (body ERCFWBAEL/L?)]
   [withS* (bindings (listof bindingS?)) (body ERCFWBAEL/L?)]
   [recS (bindings (listof bindingS?)) (body ERCFWBAEL/L?)]
   [funS (params (listof symbol?)) (body ERCFWBAEL/L?)]
   [appS (fun-expr ERCFWBAEL/L?) (args (listof ERCFWBAEL/L?))]
   [throwsS (exception-id symbol?)]
   [try/catchS (bindings (listof bindingS?)) (body ERCFWBAEL/L?)])

;; TDA para representar los árboles de sintaxis abstracta del lenguaje ERCFBAEL/L.
;; Este TDA es uan versión sin azúcar sintáctica.
(define-type ERCFBAEL/L
   [id (i symbol?)]
   [num (n number?)]
   [bool (b boolean?)]
   [lisT (elems (listof ERCFBAEL/L?))]
   [op (f procedure?) (args (listof ERCFBAEL/L?))]
   [iF (expr ERCFBAEL/L?) (then-expr ERCFBAEL/L?) (else-expr ERCFBAEL/L?)]
   [fun (params (listof symbol?)) (body ERCFBAEL/L?)]
   [rec (bindings (listof Binding?)) (body ERCFBAEL/L?)]
   [app (fun-expr ERCFBAEL/L?) (args (listof ERCFBAEL/L?))]
   [throws (exception-id symbol?)]
   [try/catch (bindings (listof Binding?)) (body ERCFBAEL/L?)])

;; TDA para representar los resultados devueltos por el intérprete.
(define-type ERCFBAEL/L-Value
   [numV (n number?)]
   [boolV (b boolean?)]
   [closureV (params (listof symbol?)) (body ERCFBAEL/L?) (env Env?)]
   [exprV (expr ERCFBAEL/L?) (env Env?)]
   [listV (elems (listof ERCFBAEL/L-Value?))]
   [exceptionV (exception-id symbol?) (continuation continuation?)])

;; TDA para asociar identificadores con valores con azúcar sintáctica.
(define-type BindingS
   [bindingS (name symbol?) (value ERCFWBAEL/L?)])

;; TDA para asociar identificadores con valores sin azúcar sintáctica.
(define-type Binding
   [binding (name symbol?) (value ERCFBAEL/L?)])

;; TDA para representar condiciones.
(define-type Condition
   [condition (expr ERCFWBAEL/L?) (then-expr ERCFWBAEL/L?)]
   [else-cond (else-expr ERCFWBAEL/L?)])

;; TDA para representar los ambientes de evaluación.
(define-type Env
   [mtSub]
   [aSub (name symbol?) (value ERCFBAEL/L-Value?) (env Env?)]
   [aRecSub (name symbol?) (value boxed-ERCFBAEL/L-Value?) (env Env?)])

;; Para trabajar con cajas que guarden el resultado de evaluación.
(define (boxed-ERCFBAEL/L-Value? v)
   (and (box? v) (ERCFBAEL/L-Value? (unbox v))))
