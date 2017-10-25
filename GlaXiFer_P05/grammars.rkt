#lang plai

;; TDA para representar los árboles de sintaxis abstracta del lenguaje CFWBAE/L. 
;; Este TDA es una versión con azúcar sintáctica.
(define-type CFWBAE/L
   [idS (i symbol?)]
   [numS (n number?)]
   [boolS (b boolean?)]
   [opS (f procedure?) (args (listof CFWBAE/L?))]
   [ifS (expr CFWBAE/L?) (then-expr CFWBAE/L?) (else-expr CFWBAE/L?)]
   [condS (cases (listof Condition?))]
   [withS (bindings (listof binding?)) (body CFWBAE/L?)]
   [withS* (bindings (listof binding?)) (body CFWBAE/L?)]
   [funS (params (listof symbol?)) (body CFWBAE/L?)]
   [appS (fun-expr CFWBAE/L?) (args (listof CFWBAE/L?))])

;; TDA para asociar identificadores con valores.
(define-type Binding
   [binding (name symbol?) (value CFWBAE/L?)])

;; TDA para representar los árboles de sintaxis abstracta del lenguaje CFBAE/L. 
;; Este TDA es una versión sin azúcar sintáctica.
(define-type CFBAE/L
   [id (i symbol?)]
   [num (n number?)]
   [bool (b boolean?)]
   [op (f procedure?) (args (listof CFBAE/L?))]
   [iF (expr CFBAE/L?) (then-expr CFBAE/L?) (else-expr CFBAE/L?)]
   [fun (params (listof symbol?)) (body CFBAE/L?)]
   [app (fun-expr CFBAE/L?) (args (listof CFBAE/L?))])

;; TDA para representar el ambiente de evaluación.
(define-type Env
   [mtSub]
   [aSub (name symbol?) (value CFBAE/L-Value?) (env Env?)])

;; TDA para representar los resultados devueltos por el intérprete.
(define-type CFBAE/L-Value
   [numV (n number?)]
   [boolV (b boolean?)]
   [closureV (params (listof symbol?)) (body CFBAE/L?) (env Env?)]
   [exprV (expr CFBAE/L?) (env Env?)])

;; TDA para representar condiciones.
(define-type Condition
   [condition (expr CFWBAE/L?) (then-expr CFWBAE/L?)]
   [else-cond (else-expr CFWBAE/L?)])
