#lang plai

;; TDA para representar los árboles de sintaxis abstracta del lenguaje FWBAE. Este TDA es una versión 
;; con azúcar sintáctica.
(define-type FWBAE
   [idS (i symbol?)]
   [numS (n number?)]
   [boolS (b boolean?)]
   [opS (f procedure?) (args (listof FWBAE?))]
   [withS (bindings (listof binding?)) (body FWBAE?)]
   [withS* (bindings (listof binding?)) (body FWBAE?)]
   [funS (params (listof symbol?)) (body FWBAE?)]
   [appS (fun-expr FWBAE?) (args (listof FWBAE?))])

;; TDA para asociar identificadores con valores.
(define-type Binding
   [binding (name symbol?) (value FWBAE?)])

;; TDA para representar los árboles de sintaxis abstracta del lenguaje FBAE. Este TDA es una versión
;; sin azúcar sintáctica.
(define-type FBAE
   [id (i symbol?)]
   [num (n number?)]
   [bool (b boolean?)]
   [op (f procedure?) (args (listof FBAE?))]
   [fun (params (listof symbol?)) (body FBAE?)]
   [app (fun-expr FBAE?) (args (listof FBAE?))])

;; TDA para representar el ambiente de evaluación.
(define-type Env
   [mtSub]
   [aSub (name symbol?) (value FBAE-Value?) (env Env?)])

;; TDA para representar los resultados devueltos por el intérprete.
(define-type FBAE-Value
   [numV (n number?)]
   [boolV (b boolean?)]
   [closureV (params (listof symbol?)) (body FBAE?) (env Env?)])
