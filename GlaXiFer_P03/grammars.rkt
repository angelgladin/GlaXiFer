#lang plai

;; TDA para representar el árbol de sintaxis abstracto del lenguaje WAE.
(define-type WAE
   [id (i symbol?)]
   [num (n number?)]
   [op (f procedure?) (args (listof WAE?))]
   [with (bindings (listof binding?)) (body WAE?)]
   [with* (bindings (listof binding?)) (body WAE?)])

;; TDA para asociar identificadores con valores.
(define-type Binding
   [binding (name symbol?) (value WAE?)])
