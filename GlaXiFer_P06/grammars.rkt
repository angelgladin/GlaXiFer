#lang plai

;; TDA para representar los árboles de sintáxis abtracta del lenguaje RCFWBAEL/L.
;; Este TDA es una versión con azúcar sintáctica.
(define-type RCFWBAEL/L
   [idS (i symbol?)]
   [numS (n number?)]
   [boolS (b boolean?)]
   [listS (elems (listof RCFWBAEL/L?))]
   [opS (f procedure?) (args (listof RCFWBAEL/L?))]
   [ifS (expr RCFWBAEL/L?) (then-expr RCFWBAEL/L?) (else-expr RCFWBAEL/L?)]
   [condS (cases (listof Condition?))]
   [withS (bindings (listof bindingS?)) (body RCFWBAEL/L?)]
   [withS* (bindings (listof bindingS?)) (body RCFWBAEL/L?)]
   [recS (bindings (listof bindingS?)) (body RCFWBAEL/L?)]
   [funS (params (listof symbol?)) (body RCFWBAEL/L?)]
   [appS (fun-expr RCFWBAEL/L?) (args (listof RCFWBAEL/L?))])

;; TDA para representar los árboles de sintaxis abstracta del lenguaje RCFBAEL/L.
;; Este TDA es uan versión sin azúcar sintáctica.
(define-type RCFBAEL/L
   [id (i symbol?)]
   [num (n number?)]
   [bool (b boolean?)]
   [lisT (elems (listof RCFBAEL/L?))]
   [op (f procedure?) (args (listof RCFBAEL/L?))]
   [iF (expr RCFBAEL/L?) (then-expr RCFBAEL/L?) (else-expr RCFBAEL/L?)]
   [fun (params (listof symbol?)) (body RCFBAEL/L?)]
   [rec (bindings (listof Binding?)) (body RCFBAEL/L?)]
   [app (fun-expr RCFBAEL/L?) (args (listof RCFBAEL/L?))])

;; TDA para representar los resultados devueltos por el intérprete.
(define-type RCFBAEL/L-Value
   [numV (n number?)]
   [boolV (b boolean?)]
   [closureV (params (listof symbol?)) (body RCFBAEL/L?) (env Env?)]
   [exprV (expr RCFBAEL/L?) (env Env?)]
   [listV (elems (listof RCFBAEL/L-Value?))])

;; TDA para asociar identificadores con valores con azúcar sintáctica.
(define-type BindingS
   [bindingS (name symbol?) (value RCFWBAEL/L?)])

;; TDA para asociar identificadores con valores sin azúcar sintáctica.
(define-type Binding
   [binding (name symbol?) (value RCFBAEL/L?)])

;; TDA para representar condiciones.
(define-type Condition
   [condition (expr RCFWBAEL/L?) (then-expr RCFWBAEL/L?)]
   [else-cond (else-expr RCFWBAEL/L?)])

;; TDA para representar los ambientes de evaluación.
(define-type Env
   [mtSub]
   [aSub (name symbol?) (value RCFBAEL/L-Value?) (env Env?)]
   [aRecSub (name symbol?) (value boxed-RCFBAEL/L-Value?) (env Env?)])

;; Para trabajar con cajas que guarden el resultado de evaluación.
(define (boxed-RCFBAEL/L-Value? v)
   (and (box? v) (RCFBAEL/L-Value? (unbox v))))
