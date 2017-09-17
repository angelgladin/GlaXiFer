#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)


#| Módulo para pruebas unitarias de la práctica 3 |#

;; Pruebas para  parse
(test (parse 'foo) (id 'foo))
(test (parse 'baz) (id 'baz))

(test (parse 666) (num 666))
(test (parse -666) (num -666))
(test (parse (/ 1 2)) (num (/ 1 2)))
(test (parse (sqrt 2)) (num (sqrt 2)))
(test (parse 1+2i) (num 1+2i))

(test (parse '(+ 1 2 3)) (op + (list (num 1) (num 2) (num 3))))
(test (parse '(- 666 666)) (op - (list (num 666) (num 666))))
(test (parse '(* 111 6)) (op * (list (num 111) (num 6))))
(test (parse '(/ 1 2 2 2)) (op / (list (num 1) (num 2) (num 2) (num 2))))
(test (parse '(% 666 2)) (op modulo (list (num 666) (num 2))))
(test (parse '(min 666 666 666 0)) (op min (list (num 666) (num 666) (num 666) (num 0))))
(test (parse '(max 666 666 666 0)) (op max (list (num 666) (num 666) (num 666) (num 0))))
(test (parse '(* 1 2 2 2)) (op * (list (num 1) (num 2) (num 2) (num 2))))
(test (parse '(pow 2 2)) (op mexpt (list (num 2) (num 2))))
(test (parse '(+ 666 (- 666 666))) (op + (list (num 666) (op - (list (num 666) (num 666))))))

(test (parse '(with (a 666)
                    (+ 666 666)))
      (with (list (binding 'a (num 666)))
            (op + (list (num 666) (num 666)))))
(test (parse '(with (a 666)
                    (with (b 0)
                          (+ a b))))
      (with (list (binding 'a (num 666)))
            (with (list (binding 'b (num 0)))
                  (op + (list (id 'a) (id 'b))))))
(test (parse '(with (a 666) (with (b a) (with (c 1) (+ a b c)))))
      (with (list (binding 'a (num 666)))
            (with (list (binding 'b (id 'a)))
                  (with (list (binding 'c (num 1)))
                        (op + (list (id 'a) (id 'b) (id 'c)))))))

(test (parse '(with* ((a 0) (b a))
                     (+ b b)))
      (with* (list (binding 'a (num 0)) (binding 'b (id 'a)))
             (op + (list (id 'b) (id 'b)))))
(test (parse '(with* ((a 0) (b 1))
                     (+ a b)))
       (with* (list (binding 'a (num 0)) (binding 'b (num 1)))
              (op + (list (id 'a) (id 'b)))))
(test (parse '(with* ((a 0) (b 1) (c 2))
                     (+ a b c)))
       (with* (list (binding 'a (num 0)) (binding 'b (num 1)) (binding 'c (num 2)))
              (op + (list (id 'a) (id 'b) (id 'c)))))
   
;; Pruebas para  interp

(test/exn (interp (parse 'foo)) "Variable Libre.")
(test/exn (interp (parse 'baz)) "Variable Libre.")
(test (interp(parse 666)) 666)
(test (interp(parse -666)) -666)
(test (interp(parse (/ 1 2))) (/ 1 2))
(test (interp(parse (sqrt 2))) (sqrt 2))
(test (interp(parse 1+2i)) 1+2i)
(test (interp(parse '(+ 1 2 3))) 6)
(test (interp(parse '(- 666 666))) 0)
(test (interp(parse '(* 111 6))) 666)
(test (interp(parse '(/ 1 2 2 2))) (/ 1 8))   
(test (interp(parse '(modulo 666 2))) 0)   
(test (interp(parse '(min 666 2 666 0))) 0)
(test (interp(parse '(max 666 2 666 0))) 666)  
(test (interp(parse '(expt 2 2))) 4)  
(test (interp(parse '(+ 666 (- 666 666)))) 666)  

(test (interp(parse '{with {a 666} {+ 666 666}})) 1332)

(test (interp(parse '{with {a 666} {with {b 0} {+ a b}}})) 666)

(test (interp(parse '{with {a 666} {with {b a} {with {c 1} {+ a b c}}}})) 1333)

(test (interp(parse '{with* {{a 0} {b a}} {+ b b}})) 0)

(test (interp(parse '{with* {{a 0} {b 1}} {+ a b}})) 1)

(test (interp(parse '{with* {{a 0} {b 1} {c 2}} {+ a b c}})) 3)


;; Pruebas para  subst (opcional)

(test (subst (op + (list (id 'a) (num 3))) 'a (num 4))
      (op + (list (num 4) (num 3))))
