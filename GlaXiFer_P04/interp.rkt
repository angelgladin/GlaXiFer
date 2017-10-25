#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta sin azúcar sintáctica generado por
;; la función desugar. Esta versión de interp usa alcance estático.
;; interp: FBAE -> FBAE-Value
(define (interp expr env)
  (match expr
    [(id i) (lookup i env)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(op f args) (opf f (foldr (λ (v l) (cons (interp v env) l)) '() args))]
    [(fun params body) (closureV params body env)]
    [(app fun-expr args)
     (let [(fun-val (interp fun-expr env))]
       (interp
        (closureV-body fun-val)
        (create-env (closureV-params fun-val) args env)))]))

;; Función auxiliar que creo un ambiente y va emparejando los parametros
;; formales con los argumentos.
(define (create-env params args env)
  (match params
    ['() env]
    [(cons x xs)
     (cond
       [(empty? args) error 'create-env "Missing arguments"]
       [else (create-env xs (cdr args) (aSub x (interp (car args) env) env))])]))

;; Función auxiliar que dada un operación y una lista de argumentos,
;; aplica dicha aperación a los argumentos
(define (opf f l)
  (let ([result (apply f (map (λ (v) (match v
                                       [(? numV?) (numV-n v)]
                                       [(? boolV?) (boolV-b v)])) l))])
    (if (list-contain? (list + - * / modulo min max mexpt) f)
        (numV result)
        (boolV result))))

;; Función auxiliar que verifica si un elemento pertenece a una lista.
;; lista-contiene: list any -> boolean
(define (list-contain? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t (list-contain? xs e))]))

;; Función auxiliar que nos dice si un identificados está asociado a un valor.
;; lookup: symbol DefrdSub -> FAE-Value
(define (lookup sub-name env)
  (match env
    [(mtSub) (error "Free identifier")]
    [(aSub name value rest-env)
     (if (symbol=? name sub-name)
         value
         (lookup sub-name rest-env))]))
