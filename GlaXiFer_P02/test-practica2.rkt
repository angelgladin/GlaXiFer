#lang plai

(require "practica2.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 2 |#

;; Pruebas para funcion->string
(test (funcion->string (x)) "x")
(test (funcion->string (mul (cte 2) (x))) "(2 * x)")
(test (funcion->string (sum (mul (cte 3) (x)) (x))) "((3 * x) + x)")
(test (funcion->string (pot (x) 9)) "x^9")
(test (funcion->string (pot (mul (cte 6) (x)) 2)) "(6 * x)^2")


;; Pruebas para evalua
(test (evalua (x) 666) (cte 666))
(test (evalua (cte 666) 123) (cte 666))
(test (evalua (mul (cte 0) (x)) 666) (mul (cte 0) (cte 666)))
(test (evalua (sum (cte 666) (x)) 666) (sum (cte 666) (cte 666)))
(test (evalua (div (x) (x)) 666) (div (cte 666) (cte 666)))


;; Pruebas para deriva
(test (deriva (x)) (cte 1))
(test (deriva (cte 666)) (cte 0))
(test (deriva (sum (mul (cte 2) (x)) (cte 666)))
      (sum (sum (mul (cte 2) (cte 1)) (mul (x) (cte 0))) (cte 0)))
(test (deriva (mul (cte 2) (x))) (sum (mul (cte 2) (cte 1)) (mul (x) (cte 0))))
(test (deriva (div (cte 1) (mul (cte 2) (x))))
      (div
       (sum
        (mul (cte 1) (sum (mul (cte 2) (cte 1)) (mul (x) (cte 0))))
        (mul (cte -1) (mul (mul (cte 2) (x)) (cte 0))))
       (pot (mul (cte 2) (x)) 2)))
(test (deriva (pot (mul (cte 2) (x)) 2))
      (mul (mul (cte 2) (pot (mul (cte 2) (x)) 1)) (sum (mul (cte 2) (cte 1)) (mul (x) (cte 0)))))


;; Pruebas para verifica
;; Se creó una función anónima que lo única que hace es devolver un símbolo constante.
(test (verifica (afd '(p q r s t) '(a b) 'p (λ () 'a) '(q))) #t)
(test (verifica (afd '(p q r) '(a b) 'r (λ () 'a) '(p q r))) #t)
(test (verifica (afd '(p q r) '(a b) 'q (λ () 'a) '(r))) #t)
(test (verifica (afd '(p q r s t) '(a b) 'a (λ () 'a) '(b))) #f)
(test (verifica (afd '(p q r) '(a b) 'b (λ () 'a) '(q))) #f)


;; Pruebas para acepta?
(test (acepta? (afd '(p q r) '(a b) 'p transicion '(q)) '(a a)) #t)
(test (acepta? (afd '(p q r) '(a b) 'p transicion '(q)) '(a)) #t)
(test (acepta? (afd '(p q r) '(a b) 'p transicion '(q)) '(a a b a)) #f)
(test (acepta? (afd '(p q r) '(a b) 'p transicion '(q)) '(b a b)) #f)
(test (acepta? (afd '(p q r) '(a b) 'p transicion '(q)) '(b b b)) #f)


;; Pruebas para calc-a
(define a (arrg number? 5 '(1 2 3 4 5)))
(test (calc-a a) (arrg number? 5 '(1 2 3 4 5)))
(define b (arrg number? 6 '(1 2 3 4 5 6)))
(test (calc-a b) (arrg number? 6 '(1 2 3 4 5 6)))
(test (calc-a (agrega-a 2 a 4)) (arrg number? 5 '(1 2 3 4 2)))
(test (calc-a (agrega-a 666 a 0)) (arrg number? 5 '(666 2 3 4 5)))
(test (calc-a (obten-a a 2)) 3)

#| ... Aquí van las pruebas (Borrar este comentario) ... |#

;; Pruebas para calc-c
#| ... Aquí van las pruebas (Borrar este comentario) ... |#
