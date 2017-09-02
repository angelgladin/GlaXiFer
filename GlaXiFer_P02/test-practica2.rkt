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

#| ... Aquí van las pruebas (Borrar este comentario) ... |#

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

#| ... Aquí van las pruebas (Borrar este comentario) ... |#

;; Pruebas para verifica

#| ... Aquí van las pruebas (Borrar este comentario) ... |#

;; Pruebas para acepta?

#| ... Aquí van las pruebas (Borrar este comentario) ... |#

;; Pruebas para calc-a

#| ... Aquí van las pruebas (Borrar este comentario) ... |#

;; Pruebas para calc-c
#| ... Aquí van las pruebas (Borrar este comentario) ... |#
