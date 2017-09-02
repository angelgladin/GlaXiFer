#lang plai

#| Práctica 1: Introducción a Racket |#

;; Función que toma dos números enteros positivos y eleva uno al otro, para luego sumar las raíces
;; cuadradas de éstos.
;; rps: number number -> number
(define (rps a b)
  (+ (sqrt (expt a b)) (sqrt (expt b a))))

;; Función que encuentra el área de un tirángulo dados sus lados, usando la fórmula de Herón. Se usa
;; la primitiva let para evitar cálculos repetitivos.
;; area-heron: number number number -> number
(define (area-heron a b c)
  (let ([s (/ (+ a b c) 2)])
    (sqrt(* s (- s a) (- s b) (- s c)))))

;; Predicado que determina si la pareja a b entará en el antro usando condicionales. La respuesta de 
;; el predicado está dada de acuerdo a lo siguiente:
;; "Si el estilo de los asistentes es de ocho o más, el predicado responderá 'si con la excepción de
;;  que si el estilo de alguno de los asistentes es de dos o menos, responderá 'no. En otro caso,
;;  responderá 'quiza."
;; entra?: number number -> symbol
(define (entra? a b)
  (cond
    [(and (> a 7) (> b 2)) 'si]
    [(and (> b 7) (> a 2)) 'si]
    [(or (< a 3) (< b 3)) 'no]
    [else 'quiza]))

;; Función recursiva que regresa el número de apariciones del dígito m como digito en el número 
;; entero positivo n.
;; apariciones: number number -> number
(define (apariciones n m)
  (letrec ([contador
            (lambda (sn sm)
              (if (non-empty-string? sn)
                  (if (equal? (substring sn 0 1) sm)
                      (+ 1 (contador (substring sn 1) sm))
                      (contador (substring sn 1) sm))
                  0))])
    (contador (number->string n) (number->string m))))

;; Función recursiva que calcula el número de pares de una cadena. Decimos que un par en una cadena
;; son dos caracteres idénticos, separados por un tercero. Por ejemplo "AxA" es el par de "A". Los
;; pares, además, pueden anidarse, por ejemplo.
;; cuenta-pares: string -> number
(define (cuenta-pares c)
  (let ([lc (string->list c)])
    (cuenta-p-aux lc)))

;; Función recursiva de cuenta-pares donde recibe una lista de caracteres para poder comparar
;; por tercias.
;; cuenta-p-aux: list -> number
(define (cuenta-p-aux l)
  (match l
    ['() 0]
    [(list a b ) 0]
    [(cons x xs) (if (and (not(equal? x (car xs))) (equal? x (cadr xs)))
                     (+ 1 (cuenta-p-aux xs))
                     (+ 0 (cuenta-p-aux xs)))]))

;; Función que imprime una piramide con n pisos haciendo uso de alguna función de impresión.
;; piramide: number -> void
(define (piramide n)
  (letrec (
           [repeat-string (lambda (n s)
                            (if (> n 0)
                                (string-append s (repeat-string (- n 1) s))
                                ""))]
           [aux (lambda (a b c)
                  (match c
                    [0 (repeat-string (- (* 2 b) 1) "*")]
                    [_ (string-append (repeat-string a " ")
                                      (repeat-string (- (* 2 b) 1) "*")
                                      "\n"
                                      (aux (- a 1) (+ b 1) (- c 1)))]))])
    (display (aux (- n 1) 1 (- n 1)))))

;; Función que recibe dos listas y construye una nueva lista con listas de longitud 2 formadas a 
;; partir de los elementos de ambas listas.
;; arma-pares: list list -> (listof list)
(define (arma-pares lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [(null? lst2) lst1]
    [(cons (list (car lst1) (car lst2)) (arma-pares (cdr lst1) (cdr lst2)))]))

;; Función que recibe una lista con elementos de la forma '(id value) y regresa el valor asociado al
;; id que fue pasado como parámetro.
;; lookup: (listof list) -> any
(define (lookup id lst)
  (cond
    [(equal? id (first (first lst))) (second (first lst))]
    [else (lookup id (cdr lst))]))

;; Función que compara la longitud de las listas lst1 y lst2. El valor de regreso son alguno de los 
;; siguientes:
;; · 'lista1-mas-grande
;; · 'lista2-mas-grande
;; · 'listas-iguales
;; compara-longitud: list list -> symbol
(define (compara-longitud lst1 lst2)
  (match (list lst1 lst2)
    [(list '() '()) 'listas-iguales]
    [(list '() _) 'lista2-mas-grande]
    [(list _ '()) 'lista1-mas-grande]
    [(list (cons x xs) (cons y ys)) (compara-longitud xs ys)]))

;; Función que entierra el símbolo nombre, n número de veces. Es decir, se anidan n - 1 listas hasta
;; que se llega a la lista que tiene al símbolo nombre.
;; entierra: symbol number -> list
(define (entierra nombre n)
  (cond
    [(= n 0) nombre]
    [(= n 1) (cons nombre empty)]
    [else (cons (entierra nombre (sub1 n)) empty )]))

;; Función que que mezcla dos listas ordenadas obtieniendo una nueva, ordenada de manera ascendente.
;; mezcla: list list -> list
(define (mezcla lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [(null? lst2) lst1]
    [(if (< (car lst1) (car lst2))
         (cons (car lst1) (mezcla (cdr lst1) lst2))
         (cons (car lst2) (mezcla (cdr lst2) lst1)))]))

;; Función que recibe una lista de números y regresa una nueva lista de cadenas que representan al
;; número binario asociado a estos números.
;; binarios: (listof number) -> (listof string)
(define (binarios lst)
  (match lst
    ['() '()]
    [(cons x xs) (cons (binarios-aux x) (binarios xs))]))

;; Función que recibe un número y regresa una cadena que representa al
;; número binario asociado a este número.
;; binarios-aux: number -> string
(define (binarios-aux x)
  (cond
    [(< x 1)(number->string x)]
    [else (string-append (binarios-aux (quotient x 2)) (number->string (modulo x 2)) )]))

;; Función que recibe una lista y regresa una nueva conteniendo únicamente aquellos que son 
;; triangulares.
;; triangulares: (listof number) -> (listof number)
(define (triangulares lst)
  (filter (lambda(x) (integer? (sqrt (+ (* 8 x) 1)))) lst))

;; Función que, usando foldr, intercala un símbolo dado entre los elementos de una lista.
;; intercalar: list symbol -> list
(define (intercalar lst s)
  (foldr (lambda (x e) (if (not(equal? e empty))
                           (concatena (list x s) e)
                           (cons x e)))
         empty lst))

;; Función que, usando foldl, intercala un símbolo dado entre los elementos de una lista.
;; intercalal: list symbol -> list
(define (intercalal lst s)
  (foldl (lambda (x e) (if (not(equal? e empty))
                           (concatena e (list s x) )
                           (cons x e)))
         empty lst))


;; Función  que  obtiene  la  concatenación  de dos  listas
;; concatena: list  list -> list
(define (concatena  lst1  lst2)
  (match  lst1
    ['() lst2]
    [(cons x xs) (cons x (concatena  xs lst2 ))]))