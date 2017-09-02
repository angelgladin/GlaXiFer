#lang plai

(require "practica1.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 1 |#

;; Pruebas para rps
(test (rps 1 7) 3.64)
(test (rps 2 9) 31.62)
(test (rps 4 0) 1)
(test (rps 5 5) 111.80)
(test (rps 0 2) 1)

;; Pruebas para area-heron
(test (area-heron 3 25 26) 36)
(test (area-heron 3 4 5) 6)
(test (area-heron 3 4 2) 2.9)
(test (area-heron 13 16 23) 100.69)
(test (area-heron 4 6 3) 5.33)

;; Pruebas para entra?
(test (entra? 5 10) 'si)
(test (entra? 3 9) 'si)
(test (entra? 5 2) 'no)
(test (entra? 11 1) 'no)
(test (entra? 5 5) 'quiza)

;; Pruebas para apariciones
(test (apariciones 717 7) 2)
(test (apariciones 2 2) 1)
(test (apariciones 123 8) 0)
(test (apariciones 1729 4) 0)
(test (apariciones 1835 0) 0)

;; Pruebas para cuenta-pares
(test (cuenta-pares "axa") 1)
(test (cuenta-pares "axax") 2)
(test (cuenta-pares "axbx") 1)
(test (cuenta-pares "") 0)
(test (cuenta-pares "axabacadad") 5)

;; No hay pruebas para piramide, revisar su impresión.

;; Pruebas para arma-pares
(test (arma-pares '(foo bar) '(10 20)) '((foo 10) (bar 20)))
(test (arma-pares '(1 7 2 9) '(1 8 3 5)) '((1 1) (7 8) (2 3) (9 5)))
(test (arma-pares '() '()) '())
(test (arma-pares '(#\a #\b #\c #\d) '("a" "b" "c" "d")) '((#\a "a") (#\b "b") (#\c "c") (#\d "d")))
(test 
   (arma-pares '(405 502 613) '("cuatroscientos cinco" "quinientos dos" "seiscientos trece"))
   '((405 "cuatroscientos cinco") (502 "quinientos dos") (613 "seiscientos trece")))

;; Pruebas para lookup
(test (lookup 'foo '((foo 10) (bar 20))) 10)
(test (lookup 'a '((a 1) (b 7) (c 2) (d 9))) 1)
(test (lookup 'c '((a 1) (b 7) (c 2) (d 9))) 2)
(test (lookup "tres" '(("uno" 1) ("dos" 2) ("tres" 3) ("cuatro" 4))) 3)
(test 
   (lookup 
      5 
      (lookup 
         'uno 
         '((uno ((5 "dos"))) 
           (dos ((4 "tres"))) 
           (tres ((3 "cuatro"))) 
           (cuatro ((2 "cinco"))) 
           (cinco ((1 "uno")))))) 
   "dos")

;; Pruebas para compara-longitud
(test (compara-longitud '(4 0) '(5)) 'lista1-mas-grande)
(test (compara-longitud '(1 2 3) '(5 6 7 8 9 10)) 'lista2-mas-grande)
(test (compara-longitud '(0 2) '(6 1 3)) 'lista2-mas-grande)
(test (compara-longitud '() '()) 'listas-iguales)
(test (compara-longitud '(1 7 2 9) '(1 8 3 5)) 'listas-iguales)

;; Pruebas para entierra
(test (entierra 'foo 0) 'foo)
(test (entierra 'foo 1) '(foo))
(test (entierra 'foo 3) '(((foo))))
(test (entierra 'bar 4) '((((bar)))))
(test (entierra 'foo 5) '(((((foo))))))

;; Pruebas para mezcla
(test (mezcla '() '(1 2 7 9)) '(1 2 7 9))
(test (mezcla '(1 3 5 8) '()) '(1 3 5 8))
(test (mezcla '(1 2 3 4 5) '(6 7 8 9 10)) '(1 2 3 4 5 6 7 8 9 10))
(test (mezcla '(6 7 8 9 10) '(1 2 3 4 5)) '(1 2 3 4 5 6 7 8 9 10))
(test (mezcla '(1 2 6 8 10 12) '(2 3 5 9 13)) '(1 2 2 3 5 6 8 9 10 12 13))

;; Pruebas para binarios
(test (binarios '()) '())
(test (binarios '(0 1 2 3 4)) '("0" "01" "010" "011" "0100"))
(test (binarios '(1 7 2 9)) '("01" "0111" "010" "01001"))
(test (binarios '(1 8 3 5)) '("01" "01000" "011" "0101"))
(test (binarios '(4 0 5 5 0 2 6 1 3)) '("0100" "0" "0101" "0101" "0" "010" "0110" "01" "011"))

;; Pruebas para triangulares
(test (triangulares '()) '())
(test (triangulares '(1 2 3 4 5 6)) '(1 3 6))
(test (triangulares '(2 4 5 7 8 9 11 12 13 14)) '())
(test 
   (triangulares '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
   '(1 3 6 10 15 21))
(test
   (triangulares (triangulares '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))
   '(1 3 6 10 15 21))

;; Pruebas para intercalar
(test (intercalar '() '+) '())
(test (intercalar '("a") '-) '("a"))
(test (intercalar '(1 2 3) '*) '(1 * 2 * 3))
(test (intercalar '(a b c d) '<>) '(a <> b <> c <> d))
(test (intercalar '((4 0 5) (5 0 2) (6 1 3)) '~) '((4 0 5) ~ (5 0 2) ~ (6 1 3)))

;; Pruebas para intercalal
(test (intercalal '() '+) '())
(test (intercalal '("a") '-) '("a"))
(test (intercalal '(1 2 3) '*) '(1 * 2 * 3))
(test (intercalal '(a b c d) '<>) '(a <> b <> c <> d))
(test (intercalal '((4 0 5) (5 0 2) (6 1 3)) '~) '((4 0 5) ~ (5 0 2) ~ (6 1 3)))
