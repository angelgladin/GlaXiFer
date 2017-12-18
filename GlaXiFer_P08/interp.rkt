#lang plai

(require "grammars.rkt")
(require "parser.rkt")

#|-----------------------------------------------------------------------------------
-                                                                                   -
-                                Análisis semántico                                 -
-                                                                                   -
-----------------------------------------------------------------------------------|#
(require racket/trace)

(define current-location (box -1))

(define (nextlocation)
  (begin
    (set-box! current-location (+ 1 (unbox current-location)))
    (unbox current-location)))

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el
;; parser. El intérprete requiere un ambiente de evaluación en esta versión para
;; buscar el valor de los identificadores.
;; interp: BERCFBAEL/L Env Store -> BERCFBAEL/L-Value
(define (interp expr env store)
  (v*s-value (interp-aux expr env store)))


;; Wrapper de la función `interp`. 
;; interp-aux: BERCFBAEL/L Env Store -> Value*Store
(define (interp-aux expr env store)
  (match expr
    [(id i) (v*s (store-lookup (env-lookup i env) store) store)]
    [(num n) (v*s (numV n) store)]
    [(bool b) (v*s (boolV b) store)]
    [(lisT elems) (let ([interpreted-args (carry-store-to-last-expr elems env store '())])
                    (v*s (listV (lv*s-value interpreted-args)) (lv*s-store interpreted-args)))]
    [(op f args) (let* ([interpreted-args (carry-store-to-last-expr args env store '())]
                        [unboxed-args (map strict (lv*s-value interpreted-args))])
                   (v*s (opf f unboxed-args) (lv*s-store interpreted-args)))]
    [(iF expr then-expr else-expr)
     (let* ([cond (strict (interp-aux expr env store))]
            [cond-value (v*s-value cond)]
            [cond-store (v*s-store cond)])
       (if (exceptionV? cond-value)
           (v*s cond-value cond-store)
           (if (boolV-b cond-value)
               (interp-aux then-expr env cond-store)
               (interp-aux else-expr env cond-store))))]
    [(fun params body) (v*s (closureV params body env) store)]
    ; Por como implementé el rec en el desugar aseguro que la cardinalidad de los
    ; bindings forzosamente será 1.
    [(rec (cons (binding name value) empty) body)
     (let* ([location (nextlocation)]
            [new-env (aSub name location env)]
            [result (interp-aux value new-env store)]
            [result-value (v*s-value result)]
            [result-store (v*s-store result)]
            [new-store (aSto location result-value result-store)])
       (interp-aux
        body
        new-env
        new-store))]
    [(app fun-expr args)
     (let* ([fun-res (strict (interp-aux fun-expr env store))]
            [fun-res-val (v*s-value fun-res)]
            [fun-res-store (v*s-store fun-res)])
       (if (exceptionV? fun-res-val)
           (v*s fun-res-val fun-res-store)
             (let* ([binded-env-store (bind-env-store (closureV-params fun-res-val)
                                                    args
                                                    (closureV-env fun-res-val)
                                                    fun-res-store)]
                  [new-env (e*s-env binded-env-store)]
                  [new-store (e*s-store binded-env-store)]
                  [closure-body (closureV-body fun-res-val)])
             (interp-aux closure-body new-env new-store))))]
    [(throws exception-id)
     (v*s (let/cc k (exceptionV exception-id k)) store)]
    [(try/catch bindings body)
     ;; INCOMPLETO :(
     (let* ([expr-body (strict (interp-aux body env))]
            ; Función anónima que devolverá el binding de la excepción, cabe
            ; resaltar que nunca devolverá 'nil porque solo se llamará en caso de
            ; que se tenga un excepción, por eso lo definí como función.
            [catched-exception (λ (id) (foldr (λ (v l) (if (symbol=? (binding-name v) id) v l)) 'nil bindings))]
            ; Función anónima que regrasa el identificador (un símbolo) de la excepción.
            [exception-id (λ () (exceptionV-exception-id expr-body))])
       (if (and (exceptionV? expr-body)
                (equal? (exceptionV-exception-id expr-body)
                        (binding-name (catched-exception (exception-id)))))
           ; En caso de que se haya detecatado una excepción
           (v*s ((exceptionV-continuation expr-body) (interp-aux (binding-value (catched-exception (exception-id))) env)) store)
           (v*s expr-body store)))]
    
    [(newbox box)
     (let* ([location (nextlocation)]
            [result (interp-aux box env store)]
            [box-value (v*s-value result)]
            [box-store (v*s-store result)])
       (v*s (boxV location) (aSto location box-value box-store)))]
    [(setbox box value)
     (let* ([boxv (interp-aux box env store)]
            [boxv-value (v*s-value boxv)]
            [boxv-store (v*s-store boxv)]
            [val (interp-aux value env boxv-store)]
            [val-value (v*s-value val)]
            [val-store (v*s-store val)])
       (v*s val-value (aSto (boxV-location boxv-value) val-value val-store)))]
    [(openbox box)
     (let* ([boxv (interp-aux box env store)]
            [box-value (v*s-value boxv)]
            [box-store (v*s-store boxv)])
       (v*s (store-lookup (boxV-location box-value) box-store) box-store))]
    [(seqn actions)
     (letrec ([carry-store-and-execute-last-action
               (λ (x store-aux) (let* ([head (car x)]
                                       [tail (cdr x)]
                                       [a1 (interp-aux head env store-aux)]
                                       [a1-store (v*s-store a1)])
                                  (match tail
                                    ['() a1]
                                    [else (carry-store-and-execute-last-action tail a1-store)])))])
       (carry-store-and-execute-last-action actions store))]))

;(trace interp-aux)

;; Función auxiliar encargada de evaluar la i-ésima expresión de una lista de
;; BERCFBAEL/L para pasarle al (i+1)-ésimo elemento el store anterior.
;; Se utilizó la técnica de recursión de cola, para esto `acc` se tomó como acumulador.
;; El acumulador será un List-Value*Store. Se hizo este tipo de dato para envolver
;; los parámetros.
;; carry-store-to-last-expr: (listof BERCFBAEL/L) Env Store list -> List-Value*Store
(define (carry-store-to-last-expr args env store acc)
  (match args
    ; Caso base donde se ha explarado toda la lista de argumento. Como se fueron
    ; agregando los elementos al acumulador en la cabeza, la lista resultante está
    ; al revés es por eso que la volteamos al final.
    ['() (lv*s (reverse acc) store)]
    ; Evaluamos el primer valor de la lista de argumentos, obtemenemos el valor y
    ; el store. Obtenemos el store porque queremos pasarle el store al (i+1)-ésimo
    ; elemento. En nuestro acumulador pegamos a la cabeza el valor del primer elemento.
    [(cons x xs)
     (let* ([first (strict (interp-aux x env store))]
            [first-val (v*s-value first)]
            [first-store (v*s-store first)])
       (carry-store-to-last-expr xs env first-store (cons first-val acc)))]))

;; Función auxiliar que va "emparejando" los parametros del ambiente con los del store.
;; Al final regresará un par con el ambiente y el store con los valores.
;; bind-env-store: (listof symbol) (listof BERCFBAEL/L) Env Store -> Env*Store
(define (bind-env-store params vals env store)
  (match params
    ['() (e*s env store)]
    [(cons head-params tail-params)
     (let* ([head-vals (car vals)]
            [tail-vals (cdr vals)]
            [location (nextlocation)]
            [new-env (aSub head-params location env)]
            [new-store (aSto location (exprV head-vals new-env) store)])
       (bind-env-store tail-params tail-vals new-env new-store))]))

;; Función que busca una dirección de memoria en el ambiente indicado.
;; env-lookup: symbol Enviroment -> number
(define (env-lookup id env)
  (match env
    [(mtSub) (error 'lookup "Free identifier")]
    [(aSub sub-id location rest-env)
     (if (symbol=? id sub-id)
         location
         (env-lookup id rest-env))]))

;; Función que busca un valor de acuerdo con la dirección de memoria asociada en el
;; store indicado.
;; lookup: symbol -> BERCFBAEL/L-Value
(define (store-lookup index store)
  (match store
    [(mtSto) (error 'lookup "Valor no almacenado")]
    [(aSto location value rest-store)
     (if (= location index)
         value
         (store-lookup index rest-store))]))

;; Función auxiliar que dada un operación y una lista de argumentos,
;; aplica dicha aperación a los argumentos.
;; En el caso de que se encuentra un excepción, la regresará.
;; opf: procedure (listof BERCFBAEL) -> BERCFBAEL/L-Value
(define (opf f l)
  ; Obtenemos una lista con la(s) excepción(es) que podría tener los elementos.
  (let ([exceptions (filter exceptionV? l)])
    (if (cons? exceptions)
        ; Regresamos la primera excepción encontrada.
        (car exceptions)
        ; Obtenemos una lista que tendrá tendrá valores tales como un número
        ; booleano o una lista.
        (let ([result (apply f (map (λ (v) (match v
                                             ; Extraemos el valor del constructor que tiene
                                             ; el tipo dato BERCFBAEL/L-Value que regresa
                                             ; el intérprete.
                                             [(? numV?) (numV-n v)]
                                             [(? boolV?) (boolV-b v)]
                                             [(? listV?) (listV-elems v)])) l))])
          ; De acuerdo el tipo de función, nos fijaremos en el contradominio de ésta
          ; para poder asociar el tipo de dato que regresará.
          (cond
            [(list-contain? (list + - * / mmodulo min max mexpt) f) (numV result)]
            [(list-contain? (list car cdr) f) (listV result)]
            [else (boolV result)])))))
   
;; Función auxiliar que verifica si un elemento pertenece a una lista.
;; lista-contiene: list any -> boolean
(define (list-contain? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t (list-contain? xs e))]))

;; Función que fuerza la evaluación de una expresión BERCFBAEL/L-Value.
;; strict: BERCFBAEL/L-Value -> BERCFBAEL/L-Value
(define (strict e)
  (match e
    [(exprV expr env) (strict (interp expr env (mtSto)))]
    [else e]))
