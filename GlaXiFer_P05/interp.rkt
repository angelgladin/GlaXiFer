#lang plai

(require "grammars.rkt")
(require "parser.rkt")

; Importarlo para habilitar el función trace
; (require racket/trace)

;; Función encargada de interpretar el árbol de sintaxis abstracta sin azúcar sintáctica generado por
;; la función desugar. Esta versión de interp usa alcance estático.
;; interp: CFBAE/L -> CFBAE/L-Value
(define (interp expr env)
  (match expr
    [(id i) (lookup i env)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(op f args) (opf f (map (λ (v) (strict (interp v env))) args))]
    [(iF expr then-expr else-expr) (if (boolV-b (strict (interp expr env)))
                                       (interp then-expr env)
                                       (interp else-expr env))]
    [(fun params body) (closureV params body env)]
    [(app fun-expr args)
     (let [(fun-val (strict (interp fun-expr env)))]
       (interp
        (closureV-body fun-val)
        (create-env (closureV-params fun-val) args env)))]))

; Función que llame para ver las llamadas recursivas
; (trace interp)

;; Función auxiliar que creo un ambiente y va emparejando los parametros
;; formales con los argumentos.
;; create-env: list closureV-params -> list CFBAE/L -> Env -> Env
(define (create-env params args env)
  (match params
    ['() env]
    [(cons x xs)
     (if (empty? args)
         (error "Missing arguments")
         (create-env xs (cdr args) (aSub x (exprV (car args) env) env)))]))

;; Función auxiliar que dada un operación y una lista de argumentos,
;; aplica dicha aperación a los argumentos
(define (opf f l)
  (let ([result (apply f (map (λ (v) (match v
                                       [(? numV?) (numV-n (strict v))]
                                       [(? boolV?) (boolV-b (strict v))])) l))])
    (if (list-contain? (list + - * / modulo min max mexpt) f)
        (numV result)
        (boolV result))))

;; Función auxiliar que verifica si un elemento pertenece a una lista.
;; lista-contiene: list any -> boolean
(define (list-contain? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t (list-contain? xs e))]))

;; Función que fuerza la evaluación de una expresión CFBAE/L-Value.
;; strict: CFBAE/L-Value -> CFBAE/L-Value
(define (strict e)
  (match e
    [(exprV expr env) (strict (interp expr env))]
    [else e]))

;; Función auxiliar que nos dice si un identificados está asociado a un valor.
;; lookup: symbol -> CFBAE/L-Value
(define (lookup sub-name env)
  (match env
    [(mtSub) (error "Free identifier")]
    [(aSub name value rest-env)
     (if (symbol=? name sub-name)
         value
         (lookup sub-name rest-env))]))
