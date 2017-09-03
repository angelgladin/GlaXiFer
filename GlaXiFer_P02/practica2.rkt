#lang plai

#| Práctica 2: Tipos de datos abstractos |#

;; TDA para representar funciones.
;; Se tienen constructores para la variable independiente, constantes, suma de funciones, 
;; multiplicación de funciones, división de funciones y potencia.
(define-type funcion
  [x]
  [cte (n number?)]
  [sum (f funcion?) (g funcion?)]
  [mul (f funcion?) (g funcion?)]
  [div (f funcion?) (g funcion?)]
  [pot (f funcion?) (n number?)])

;; Función que regresa una cadena de caracteres, representando a la función que recibe como parámetro.
;; funcion->string: funcion -> string
(define (funcion->string f)
  (match f
    [(x) "x"]
    [(cte n) (number->string n)]
    [(sum f g) (string-append "(" (funcion->string f) " + " (funcion->string g) ")")]
    [(mul f g) (string-append "(" (funcion->string f) " * " (funcion->string g) ")")]
    [(div f g) (string-append "(" (funcion->string f) " / " (funcion->string g) ")")]
    [(pot f n) (string-append (funcion->string f) "^" (number->string n))]))

;; Función que devuelve el resultado de evaluar la función pasada como parámetro con el valor v, es
;; decir, regresa f(v).
;; evalua: funcion number -> funcion
(define (evalua f v)
  (match f
    [(x) (cte v)]
    [(cte n) (cte n)]
    [(sum f g) (sum (evalua f v) (evalua g v))]
    [(mul f g) (mul (evalua f v) (evalua g v))]
    [(div f g) (div (evalua f v) (evalua g v))] 
    [(pot f n) (pot (evalua f v) n)]))

;; Función que regresa la derivada de una función.
;; deriva: funcion -> funcion
(define (deriva f)
  (match f
    [(x) (cte 1)]
    [(cte n) (cte 0)]
    [(sum f g) (sum (deriva f) (deriva g))]
    [(mul f g) (sum (mul f (deriva g)) (mul g (deriva f)))]
    [(div f g) (div (sum (mul f (deriva g)) (mul (cte -1) (mul g (deriva f)))) (pot g 2))]
    [(pot f n) (mul (mul (cte n) (pot f (sub1 n))) (deriva f))]))

; ------------------------------------------------------------------------------------------------ ;

;; TDA para representar autómatas finitos deterministas.
;; Se tiene un único constructor para representar autómatas finitos deterministas como la quintúpla
;; correspondiente.
(define-type automata
  [afd (estados (listof symbol?))
       (alfabeto (listof symbol?))
       (inicial symbol?)
       (transicion procedure?)
       (finales (listof symbol?))])

;; Función auxiliar que verifica si un elemento pertenece a una lista.
;; contiene: list any -> boolean
(define (contiene? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t
                     (contiene? xs e))]))

;; Función que verifica que un afd está bien construido. Verifica que el símbolo inicial y el conjunto
;; de símbolos finales pertenecen al conjunto de estados.
;; verifica: automata -> boolean
(define (verifica atm)
   (type-case automata atm
     [afd (estados _ q0 _1 f)
          (letrec ([sub-conj? (λ (l1 l2)
                                (match l1
                                  ['() #t]
                                  [(cons x xs) (and (contiene? l2 x) (sub-conj? xs l2))]))])
            (and (contiene? estados q0) (sub-conj? f estados)))]))

;; Función de transición de un autómana, en este caso ésta es la que está en
;; la especificación de la tarea.
;; transicion: symbol -> symbol
(define (transicion estado simbolo)
  (match estado
    ['p (if (symbol=? 'a simbolo) 'q 'r)]
    ['q (if (symbol=? 'a simbolo) 'q 'r)]
    ['r 'r]))

;; Predicado que dado un afd y una lista de símbolos que representan una expresión, deterimina si es
;; aceptada por el autómata.
(define (acepta? atm lst)
   (type-case automata atm
     [afd (e a q0 transicion finales)
          (letrec ([ult (λ (q cad)
                          (match cad
                            [(cons x '()) (transicion q x)]
                            [(cons x xs) (ult (transicion q x) xs)]))])
            (contiene? finales (ult q0 lst)))]))

; ------------------------------------------------------------------------------------------------ ;

;; TDA para representar una gramática de arreglos.
;; Se tienen constructores que permiten definir un arreglo, para especificar la operación de agregar 
;; un elemento y un tercero para obtener elemento.
(define (any? a) #t)
(define-type arreglo
  [arrg (tipo procedure?) (dim integer?) (elems (listof any?))]
  [agrega-a (e any?) (a arreglo?) (i integer?)]
  [obten-a (a arreglo?) (i integer?)])

;; Función auxiliar que devuelve el arreglo anidado de agrega-a y obten-a
;; obtener-lista-anidada: arr -> list
(define (obtener-lista-anidada arr)
  (type-case arreglo arr
    [arrg (t n elems) elems]
    [agrega-a (e a i) (obtener-lista-anidada a)]
    [obten-a (a i) (obtener-lista-anidada a)]))

;; Función auxiliar que devuelve el tipo de dato anidado de agrega-a y obten-a
;; obtener-tipo-dato: arr -> arr
(define (obtener-tipo-dato arr)
  (type-case arreglo arr
    [arrg (t n elems) t]
    [agrega-a (e a i) (obtener-tipo-dato a)]
    [obten-a (a i) (obtener-tipo-dato a)]))

;; Función auxiliar que modifica la i-ésima entrada de la lista y pone el elemento dado.
;; agrega-aux: list -> list
(define (agrega-aux pos e l)
  (if (= pos 0)
      (cons e (cdr l))
      (cons (car l) (agrega-aux (sub1 pos) e (cdr l)))))

;; Función que evalúa expresiones de tipo arreglo.
;; calc-a: arreglo -> arreglo
(define (calc-a arr)
  (type-case arreglo arr
    [arrg (tipo dim elems) (cond
                             [(not (equal? dim (length elems))) (error "Dimensión inválida")]
                             [(not (equal? elems (filter tipo elems))) (error "Los elementos no son del tipo especificado.")]
                             [arr])]
    [agrega-a (e a i) (let ([arreglo-anidado (obtener-lista-anidada a)])
                        (cond
                          [(not (and (>= i 0) (< i (length arreglo-anidado)))) (error "Índice inválido")]
                          [(not ((obtener-tipo-dato a) e)) (error "Los elementos no son del tipo especificado.")]
                          [(arrg (obtener-tipo-dato a) (length arreglo-anidado) (agrega-aux i e arreglo-anidado))]))]
    [obten-a (a i) (let ([arreglo-anidado (obtener-lista-anidada a)])
                     (if (and (>= i 0) (< i (length arreglo-anidado)))
                         (list-ref arreglo-anidado i)
                         (error "índice inválido")))]))


; ------------------------------------------------------------------------------------------------ ;

;; TDA para representar una gramática de conjuntos.
;; Se tienen constructores que permiten definir un conjunto, determinar si el conjunto es vacío, 
;; determinar si un elemento está contenido en el conjunto, agregar un elemento, unir conjunto, 
;; intersectar conjunto y calcular la diferencia.
(define-type conjunto
   [tipo-conjunto-no-implementado])

;; Función que evalúa expresiones de tipo conjunto.
;; calc-c: conjunto -> conjunto
(define (calc-c cto)
   (error 'calc-c "Función no implementada"))
