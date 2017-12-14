#lang plai

(require "grammars.rkt")
(require "parser.rkt")

#|-----------------------------------------------------------------------------------
-                                                                                   -
-                                Análisis semántico                                 -
-                                                                                   -
-----------------------------------------------------------------------------------|#

(define current-location (box -1))

(define (nextlocation)
  (begin
    (set-box! current-location (+ 1 (unbox current-location)))
    (unbox current-location)))

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el
;; parser. El intérprete requiere un ambiente de evaluación en esta versión para
;; buscar el valor de los identificadores.
;; interp: BERCFBAEL/L Env Store -> ERCFBAEL/L-Value
(define (interp expr env store)
 ; (v*s-value
  (interp-aux expr env store));))

;; Wrapper del `interp`. 
;; interp-aux: BERCFBAEL/L Env Store -> Value*Store
(define (interp-aux expr env store)
    (match expr
    [(id i) (v*s (store-lookup (env-lookup i env) store) store)]
    [(num n) (v*s (numV n) store)]
    [(bool b) (v*s (boolV b) store)]
    [(lisT elems) (let ([interpreted-args (carry-store-to-last-expr elems env store '())])
                    (v*s (listV (lv*s-value interpreted-args)) (lv*s-store interpreted-args)))]
    [(op f args) (let ([interpreted-args (carry-store-to-last-expr args env store '())])
                   (v*s (opf f (lv*s-value interpreted-args)) (lv*s-store interpreted-args)))]
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

    [(rec bindings body)
     (let* ([location (nextlocation)]
            [new-env (aRecSub (binding-name (second bindings)) location env)]
            [args (list (binding-value (second bindings)))]
            [result (interp-aux (app (binding-value (first bindings)) args) env store)]
            [result-value (v*s-value result)]
            [result-store (v*s-store result)]
            [new-store (aSto location result-value result-store)])
       (interp-aux body new-env new-store))]
      
    [(app fun-expr args)
     (let* ([fun-res (strict (interp-aux fun-expr env store))]
            [fun-res-val (v*s-value fun-res)]
            [fun-res-store (v*s-store fun-res)]
            [interpreted-args (carry-store-to-last-expr args env fun-res-store '())]
            [interpreted-args-val (lv*s-value interpreted-args)]
            [interpreted-args-store (lv*s-store interpreted-args)]
            [location (nextlocation)])
      (if (exceptionV? fun-res-val)
           (v*s fun-res-val fun-res-store)
           (interp-aux
            (closureV-body fun-res-val)
            (create-env (closureV-params fun-res-val) interpreted-args-val env location)
            (aSto
             location
             interpreted-args-val
             interpreted-args-store))))]
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
           ((exceptionV-continuation expr-body) (interp-aux (binding-value (catched-exception (exception-id))) env))
           expr-body))]
    
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

;; Función auxiliar encargada de evaluar la i-ésima expresión de una lista de
;; BERCFBAEL/L para pasarle al (i+1)-ésimo elemento el store anterior.
;; Se utilizó la técnica de recursión de cola, para esto `acc` se tomó como acumulador.
;; El acumulador será una lista de BERCFBAEL/L-Value
;; carry-store-to-last-expr: (listof BERCFBAEL/L) Env Store list -> List-Value*Store
(define (carry-store-to-last-expr args env store acc)
  (match args
    ; Caso base donde se ha explorado toda la lista de argumento. Como se fueron
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


;; Función auxiliar que nos devuelve el valor asociado a un identificador.
;; lookup: symbol -> BERCFBAEL/L-Value
(define (store-lookup index store)
  (match store
    [(mtSto) (error 'lookup "Valor no almacenado")]
    [(aSto location value rest-store)
     (if (= location index)
         value
         (store-lookup index rest-store))]))

;; Función que busca un valor en el ambiente indicado.
;; env-lookup: symbol Enviroment -> number
(define (env-lookup id env)
  (match env
    [(mtSub) (error 'lookup "Identificador libre")]
    [(aSub sub-id location rest-env)
     (if (symbol=? id sub-id)
         location
         (env-lookup id rest-env))]
    [(aRecSub sub-id location rest-env)
     (if (symbol=? id sub-id)
         location
         (env-lookup id rest-env))]))


;; Función auxiliar que dada un operación y una lista de argumentos,
;; aplica dicha aperación a los argumentos.
;; En el caso de que se encuentra un excepción, la regresará.
;; opf: procedure list -> ERCFBAEL/L-Value
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
            [else (boolV result)])))))
   
;; Función auxiliar que verifica si un elemento pertenece a una lista.
;; lista-contiene: list any -> boolean
(define (list-contain? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t (list-contain? xs e))]))

;; Función que fuerza la evaluación de una expresión CFBAE/L-Value.
;; strict: CFBAE/L-Value -> ERCFBAEL/L-Value
(define (strict e)
  (match e
    [(exprV expr env) (strict (interp expr env))]
    [else e]))

;; Función auxiliar que crea un ambiente y va emparejando los parametros
;; formales con los argumentos.
;; create-env: (listof symbol) (listof BERCFBAEL/L)  Env Integer -> Env
(define (create-env params args env location)
  (match params
    ['() env]
    [(cons x xs)
     (if (empty? args)
         (error "Missing arguments")
         (create-env xs (cdr args) (aSub x (exprV (car args) env) env) location))]))
