#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 4 |#

;; Pruebas para  parse

(test (parse 'foo) (idS 'foo))
(test (parse 'baz) (idS 'baz))

(test (parse -666) (numS -666))
(test (parse (/ 1 2)) (numS (/ 1 2)))
(test (parse (sqrt 2)) (numS (sqrt 2)))
(test (parse 1+2i) (numS 1+2i))

(test (parse 'false) (boolS #f))
(test (parse 'true) (boolS #t))

(test (parse '(+ 1 2 3)) (opS + (list (numS 1) (numS 2) (numS 3))))
(test (parse '(- 666 666)) (opS - (list (numS 666) (numS 666))))
(test (parse '(* 111 6)) (opS * (list (numS 111) (numS 6))))
(test (parse '(/ 1 2 2 2)) (opS / (list (numS 1) (numS 2) (numS 2) (numS 2))))
(test (parse '(% 666 2)) (opS modulo (list (numS 666) (numS 2))))
(test (parse '(min 666 666 666 0)) (opS min (list (numS 666) (numS 666) (numS 666) (numS 0))))
(test (parse '(max 666 666 666 0)) (opS max (list (numS 666) (numS 666) (numS 666) (numS 0))))
(test (parse '(* 1 2 2 2)) (opS * (list (numS 1) (numS 2) (numS 2) (numS 2))))
(test (parse '(+ 666 (- 666 666))) (opS + (list (numS 666) (opS - (list (numS 666) (numS 666))))))

(test (parse '(with ((a 666))
                    (+ 666 666)))
      (withS (list (binding 'a (numS 666)))
            (opS + (list (numS 666) (numS 666)))))
(test (parse '(with ((a 666))
                    (with ((b 0))
                          (+ a b))))
      (withS (list (binding 'a (numS 666)))
            (withS (list (binding 'b (numS 0)))
                  (opS + (list (idS 'a) (idS 'b))))))
(test (parse '(with ((a 666) (b 0) (c 1))
                    (+ a b c)))
      (withS (list (binding 'a (numS 666)) (binding 'b (numS 0)) (binding 'c (numS 1)))
            (opS + (list (idS 'a) (idS 'b) (idS 'c)))))

(test (parse '(with* ((a 0) (b a))
                     (+ b b)))
      (withS* (list (binding 'a (numS 0)) (binding 'b (idS 'a)))
             (opS + (list (idS 'b) (idS 'b)))))
(test (parse '(with* ((a 0) (b 1))
                     (+ a b)))
       (withS* (list (binding 'a (numS 0)) (binding 'b (numS 1)))
              (opS + (list (idS 'a) (idS 'b)))))
(test (parse '(with* ((a 0) (b 1) (c 2))
                     (+ a b c)))
       (withS* (list (binding 'a (numS 0)) (binding 'b (numS 1)) (binding 'c (numS 2)))
              (opS + (list (idS 'a) (idS 'b) (idS 'c)))))

(test (parse '{fun {x} {+ x 2}})
      (funS '(x) (opS + (list (idS 'x) (numS 2)))))
(test (parse '(fun {z} (with ((a 666) (b 0) (c 1)) (+ a b c))))
      (funS '(z) (withS (list (binding 'a (numS 666)) (binding 'b (numS 0)) (binding 'c (numS 1)))
            (opS + (list (idS 'a) (idS 'b) (idS 'c))))))

(test (parse '{app a {-666 foo}})
      (appS (idS 'a) (list (numS -666) (idS 'foo))))
(test (parse '(app {fun {a} {+ a 4}} {-666 baz {+ 1 2}}))
      (appS (funS '(a) (opS + (list (idS 'a) (numS 4)))) (list (numS -666) (idS 'baz) (opS + (list (numS 1) (numS 2))))))
(test (parse '{app false {{with {{a 666}} {+ a 0}} true}})
      (appS (boolS #f) (list (withS (list (binding 'a (numS 666))) (opS + (list (idS 'a) (numS 0))))
                             (boolS #t))))

;; Pruebas para  desugar

(test (desugar (parse -666)) (num -666))
(test (desugar (parse '(with ((a 666))
                    (+ 666 666))))
      (app (fun '(a) (op + (list (num 666) (num 666)))) (list (num 666))))
(test (desugar (parse '(with* ((a 0) (b a))
                     (+ b b))))
      (app (fun '(a) (app (fun '(b) (op + (list (id 'b) (id 'b)))) (list (id 'a)))) (list (num 0))))

(test (desugar (parse '(with* ((a 0) (b 1))
                     (+ a b))))
      (app (fun '(a) (app (fun '(b) (op + (list (id 'a) (id 'b)))) (list (num 1)))) (list (num 0))))

(test (desugar(parse '(with ((a 666) (b 0) (c 1))
                    (+ a b c))))
      (app (fun '(a b c) (op + (list (id 'a) (id 'b) (id 'c)))) (list (num 666) (num 0) (num 1))))



;; Pruebas para  interp

#| ... Aquí van las pruebas (Borrar este comentario) ... |#
