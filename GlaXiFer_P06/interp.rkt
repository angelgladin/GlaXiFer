#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser. El
;; intérprete requiere un ambiente de evaluación en esta versión para buscar el valor de los 
;; identificadores.
;; interp: RCFBAEL/L Env -> RCFBAEL/L-Value
(define (interp expr env)
	(match expr
          [(id i) (lookup i env)] ;Simbolo
          [(num n) (numV n)] ;Número
          [(bool b) (boolV b)] ;Booleanin
          [(lisT elems) (listV (map (λ (v) (strict (interp v env))) elems))] ; Lista
          [(op f args) (opf f (map (λ (v) (strict (interp v env))) args))]
          [(iF expr then-expr else-expr) (if (boolV-b (strict (interp expr env)))
                                       (interp then-expr env)
                                       (interp else-expr env))]
          [(fun params body) (closureV params body env)]
          [(app fun-expr args)
           (let [(fun-val (strict (interp fun-expr env)))]
             (interp
              (closureV-body fun-val)
              (create-env (closureV-params fun-val) args env)))]
         ))

;; Función auxiliar que nos dice si un identificados está asociado a un valor.
;; lookup: symbol -> CFBAE/L-Value
(define (lookup sub-name env)
  (match env
    [(mtSub)  (error "Free identifier")]
    [(aSub name value rest-env)
     (if (symbol=? name sub-name)
         value
         (lookup sub-name rest-env))]))

;; Función auxiliar que dada un operación y una lista de argumentos,
;; aplica dicha aperación a los argumentos
;; opf: procedure -> list -> CFBAE/L-Value
(define (opf f l)
  (let ([result (apply f (map (λ (v) (match v
                                       [(? numV?) (numV-n v)]
                                       [(? boolV?) (boolV-b v)]
                                       [(? listV?) (listV-elems v)])) l))])
    (if (list-contain? (list + - * / mmodulo min max mexpt) f)
        (numV result)
        (if (list-contain? (list car cdr) f)
            result
            (boolV result)))))

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

