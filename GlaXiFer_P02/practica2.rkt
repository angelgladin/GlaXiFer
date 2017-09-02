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
   [tipo-automata-no-implementado])

;; Función que verifica que un afd está bien construido. Verifica que el símbolo inicial y el conjunto
;; de símbolos finales pertenecen al conjunto de estados.
;; verifica: automata -> boolean
(define (verifica atm)
   (error 'verifica "Función no implementada"))

;; Predicado que dado un afd y una lista de símbolos que representan una expresión, deterimina si es
;; aceptada por el autómata.
(define (acepta? atm lst)
   (error 'acepta? "Función no implementada"))

; ------------------------------------------------------------------------------------------------ ;

;; TDA para representar una gramática de arreglos.
;; Se tienen constructores que permiten definir un arreglo, para especificar la operación de agregar 
;; un elemento y un tercero para obtener elemento.
(define-type arreglo
   [tipo-arreglo-no-implementado])

;; Función que evalúa expresiones de tipo arreglo.
;; calc-a: arreglo -> arreglo
(define (calc-a arr)
   (error 'calc-a "Función no implementada"))

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
