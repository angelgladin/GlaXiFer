#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser. El
;; intérprete requiere un ambiente de evaluación en esta versión para buscar el valor de los 
;; identificadores.
;; interp: ERCFBAEL/L Env -> ERCFBAEL/L-Value
(define (interp expr env)
  (match expr
    [(id i) (lookup i env)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(lisT elems) (listV (map (λ (v) (strict (interp v env))) elems))]
    [(op f args) (opf f (map (λ (v) (strict (interp v env))) args))]
    [(iF expr then-expr else-expr) (if (boolV-b (strict (interp expr env)))
                                       (interp then-expr env)
                                       (interp else-expr env))]
    [(fun params body) (closureV params body env)]
    [(rec bindings body) (interp body (cyclically-bind-and-interp bindings env))]
    [(app fun-expr args)
     (let [(fun-val (strict (interp fun-expr env)))]
       (interp
        (closureV-body fun-val)
        (create-env (closureV-params fun-val) args env)))]))

;; Función auxiliar que nos dice si un identificados está asociado a un valor.
;; lookup: symbol -> CFBAE/L-Value
(define (lookup sub-name env)
  (match env
    [(mtSub)  (error "Free identifier")]
    [(aSub name value rest-env)
     (if (symbol=? name sub-name)
         value
         (lookup sub-name rest-env))]
    [(aRecSub name value rest-env)
     (if (symbol=? name sub-name)
         (unbox value) ; Se obtiene un closure que hace referencia al mismo ambiente.
         (lookup sub-name rest-env))]))

;; Función auxiliar que dada un operación y una lista de argumentos,
;; aplica dicha aperación a los argumentos
;; opf: procedure -> list -> CFBAE/L-Value
(define (opf f l)
  ; Obtenemos una lista que tendrá tendrá valores tales como un número
  ; booleano o una lista.
  (let ([result (apply f (map (λ (v) (match v
                                       ; Extraemos el valor del constructor que tiene
                                       ; el tipo dato RCFBAEL/L-Value que regresa
                                       ; el intérprete.
                                       [(? numV?) (numV-n v)]
                                       [(? boolV?) (boolV-b v)]
                                       [(? listV?) (listV-elems v)])) l))])
    ; De acuerdo el tipo de función, nos fijaremos en el contradominio de ésta
    ; para poder asociar el tipo de dato que regresará.
    (cond
      [(list-contain? (list + - * / mmodulo min max mexpt) f) (numV result)]
      [(list-contain? (list car cdr) f) (listV result)]
      [else (boolV result)])))
   
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
(require racket/trace)

;; Función que crea el ambiente recursivo.
;; cyclically-bind-and-interp: listof Binding -> Enviroment
(define (cyclically-bind-and-interp bindings env)
  (if (empty? bindings)
      env
      ; Creamos la caja que almacenará la función recursiva
      (let* ([value-holder (box (numV 1729))]
             ; Creamos el ambiente y lo asociamos a la caja anterior.
             [new-env (aRecSub (binding-name (car bindings)) value-holder env)]
             ; Interpretamos el valor, esto nos devuelve un clousure que tendrá como ambiente el new-env
             [named-expr-val (interp (binding-value (car bindings)) new-env)])
        (begin
          ; Modificamos la caja. Ahora en lugar de almacenar 1729 almacena el closure con la función
          ; recursiva.
          (set-box! value-holder named-expr-val)
          ; Regresamos el ambiente. La caja del ambiente tiene la nueva modificación.
          (cyclically-bind-and-interp (cdr bindings) new-env)))))

