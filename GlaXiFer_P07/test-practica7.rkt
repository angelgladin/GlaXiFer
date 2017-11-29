#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 7 |#

;; Expresión recursiva
(define expr-rec
  '{rec {
         {fac {fun {n}
                   {if {zero? n}
                       1
                       {* n {fac {- n 1}}}}}}
         {n 5}}
     {fac n}})

;; Pruebas para parse
(test (parse 'foo) (idS 'foo))
(test (parse 'baz) (idS 'baz))

(test (parse -666) (numS -666))
(test (parse (/ 1 2)) (numS (/ 1 2)))
(test (parse (sqrt 2)) (numS (sqrt 2)))
(test (parse 1+2i) (numS 1+2i))

(test (parse 'false) (boolS #f))
(test (parse 'true) (boolS #t))

(test (parse '{list 1 2 3}) (listS (list (numS 1) (numS 2) (numS 3))))
(test (parse '{list}) (listS '()))

(test (parse '{+ 1 2 3}) (opS + (list (numS 1) (numS 2) (numS 3))))
(test (parse '{- 666 666}) (opS - (list (numS 666) (numS 666))))
(test (parse '{* 111 6}) (opS * (list (numS 111) (numS 6))))
(test (parse '{/ 1 2 2 2}) (opS / (list (numS 1) (numS 2) (numS 2) (numS 2))))
(test (parse '{% 666 2}) (opS mmodulo (list (numS 666) (numS 2))))
(test (parse '{min 666 666 666 0}) (opS min (list (numS 666) (numS 666) (numS 666) (numS 0))))
(test (parse '{max 666 666 666 0}) (opS max (list (numS 666) (numS 666) (numS 666) (numS 0))))
(test (parse '{* 1 2 2 2}) (opS * (list (numS 1) (numS 2) (numS 2) (numS 2))))
(test (parse '{+ 666 {- 666 666}}) (opS + (list (numS 666) (opS - (list (numS 666) (numS 666))))))
(test (parse '{empty? {list 666}}) (opS empty? (list (listS (list (numS 666))))))
(test (parse '{head {list 666}}) (opS car (list (listS (list (numS 666))))))

(test (parse '{with {{a 666}} {+ 666 666}})
      (withS (list (bindingS 'a (numS 666)))
             (opS + (list (numS 666) (numS 666)))))
(test (parse '{with {{a 666}}
                    {with {{b 0}}
                          {+ a b}}})
      (withS (list (bindingS 'a (numS 666)))
             (withS (list (bindingS 'b (numS 0)))
                    (opS + (list (idS 'a) (idS 'b))))))
(test (parse '{with {{a 666} {b 0} {c 1}}
                    {+ a b c}})
      (withS (list (bindingS 'a (numS 666)) (bindingS 'b (numS 0)) (bindingS 'c (numS 1)))
             (opS + (list (idS 'a) (idS 'b) (idS 'c)))))

(test (parse '{with* {{a 0} {b a}}
                     {+ b b}})
      (withS* (list (bindingS 'a (numS 0)) (bindingS 'b (idS 'a)))
              (opS + (list (idS 'b) (idS 'b)))))
(test (parse '{with* {{a 0} {b 1}}
                     {+ a b}})
      (withS* (list (bindingS 'a (numS 0)) (bindingS 'b (numS 1)))
              (opS + (list (idS 'a) (idS 'b)))))
(test (parse '{with* {{a 0} {b 1} {c 2}}
                     {+ a b c}})
      (withS* (list (bindingS 'a (numS 0)) (bindingS 'b (numS 1)) (bindingS 'c (numS 2)))
              (opS + (list (idS 'a) (idS 'b) (idS 'c)))))

(test (parse expr-rec)
      (recS
       (list
        (bindingS 'fac (funS '(n) (ifS
                                   (opS zero? (list (idS 'n)))
                                   (numS 1)
                                   (opS * (list (idS 'n)
                                                (appS (idS 'fac) (list (opS - (list (idS 'n) (numS 1))))))))))
        (bindingS 'n (numS 5)))
       (appS (idS 'fac) (list (idS 'n)))))

(test (parse '{fun {x} {+ x 2}})
      (funS '(x) (opS + (list (idS 'x) (numS 2)))))
(test (parse '{fun {z} {with {{a 666} {b 0} {c 1}} {+ a b c}}})
      (funS '(z) (withS (list (bindingS 'a (numS 666)) (bindingS 'b (numS 0)) (bindingS 'c (numS 1)))
                        (opS + (list (idS 'a) (idS 'b) (idS 'c))))))

(test (parse '{{fun {a b} {+ a b}} {2 3}})
      (appS (funS '(a b) (opS + (list (idS 'a) (idS 'b)))) (list (numS 2) (numS 3))))
(test (parse '{{fun {a} {+ a 0}} {666}})
      (appS (funS '(a) (opS + (list (idS 'a) (numS 0)))) (list (numS 666))))
(test (parse '{{fun {a b c} {or a b c}} {false true false}})
      (appS (funS '(a b c) (opS or-aux (list (idS 'a) (idS 'b) (idS 'c)))) (list (boolS #f) (boolS #t) (boolS #f))))

(test (parse '{if {< 2 3} 4 5}) (ifS (opS < (list (numS 2) (numS 3))) (numS 4) (numS 5)))
(test (parse '{if {= x true} (+ 9 10) 8}) (ifS (opS = (list (idS 'x) (boolS #t))) (opS + (list (numS 9) (numS 10))) (numS 8)))
(test (parse '{if {= x 7}
                  {with* {{a 0} {b 1}}
                         {+ a b}}
                  {with* {{a 0} {b 1} {c 2}}
                         {+ a b c}}})
      (ifS (opS = (list (idS 'x) (numS 7)))
           (withS* (list (bindingS 'a (numS 0)) (bindingS 'b (numS 1)))
                   (opS + (list (idS 'a) (idS 'b))))
           (withS* (list (bindingS 'a (numS 0)) (bindingS 'b (numS 1)) (bindingS 'c (numS 2)))
                   (opS + (list (idS 'a) (idS 'b) (idS 'c))))))

(test (parse '{cond {{< 2 3} 1} {{> 10 2} 2} {else 3}})
      (condS
       (list
        (condition (opS < (list (numS 2) (numS 3))) (numS 1))
        (condition (opS > (list (numS 10) (numS 2))) (numS 2))
        (else-cond (numS 3)))))


;; Pruebas para  desugar
(test (desugar (parse '{with* {{a 0} {b 1} {c 2}}{+ a b c}}))
      (app (fun '(a) (app (fun '(b) (app (fun '(c) (op + (list (id 'a) (id 'b) (id 'c)))) (list (num 2)))) (list (num 1)))) (list (num 0))))
(test (desugar (parse '{fun {x} {+ x 2}}))
      (fun '(x) (op + (list (id 'x) (num 2)))))
(test (desugar (parse '{with {{a 3}} {+ a 4}}))
      (app (fun '(a) (op + (list (id 'a) (num 4)))) (list (num 3))))
(test (desugar (parse '{with {{a 2}}{with {{b {+ a a}}}b}}))
      (app (fun '(a) (app (fun '(b) (id 'b)) (list (op + (list (id 'a) (id 'a)))))) (list (num 2))))
(test (desugar (parse '{cond{{< 2 3} 1}{{> 10 2} 2} {else 3}}))
      (iF (op < (list (num 2) (num 3))) (num 1) (iF (op > (list (num 10) (num 2))) (num 2) (num 3))))


;; Pruebas para  interp
(test/exn (interp (desugar (parse 'foo)) (mtSub)) "Free identifier")
(test (interp (desugar (parse '1729)) (mtSub)) (numV 1729))
(test (interp (desugar (parse 'true)) (mtSub)) (boolV #t))
(test (interp (desugar (parse '{/= 1 2 3 4 5})) (mtSub)) (boolV #t))
(test (interp (desugar (parse '{if true true false})) (mtSub)) (boolV #t))
(test (interp (desugar (parse '{if {/= 2 2} true false})) (mtSub)) (boolV #f))
(test (interp (desugar (parse '{cond {true 1} {true 2} {else 3}})) (mtSub)) (numV 1))
(test (interp (desugar (parse '{with {{a 2} {b 3}} {+ a b}})) (mtSub)) (numV 5))
(test (interp (desugar (parse '{with {{a 666} {b {/ 1 0}}} {+ a 0}})) (mtSub)) (numV 666))
(test (interp (desugar (parse '{with {{a {+ 1 1}}} a})) (mtSub))
      (exprV (op + (list (num 1) (num 1))) (mtSub)))
(test (interp (desugar (parse '{with* {{a 2} {b {+ a a}}} b})) (mtSub))
      (exprV (op + (list (id 'a) (id 'a)))
             (aSub 'a (exprV (num 2) (mtSub)) (mtSub))))
(test (interp (desugar (parse '{fun {x} {+ x 2}})) (mtSub)) (closureV '(x) (op + (list (id 'x) (num 2))) (mtSub)))
(test (interp (desugar (parse '{{fun {a b} {+ a b}} {2 3}})) (mtSub)) (numV 5))
(test (interp (desugar (parse '{or {not true} false})) (mtSub)) (boolV #f))
(test (interp (desugar (parse '{with* {{a 2} {g {fun {x} {+ x a}}}} {g 3}})) (mtSub)) (numV 5))
(test (interp (desugar (parse '{pow 2 2 2 2})) (mtSub)) (numV 256))
(test (interp (desugar (parse '{% 5 10 20 2})) (mtSub)) (numV 1))
(test (interp (desugar (parse expr-rec)) (mtSub)) (numV 120))
(test (interp (desugar (parse '{tail {list 666 666}})) (mtSub)) (listV (list (numV 666))))
(test (interp (desugar (parse '{empty? {list 666}})) (mtSub)) (boolV #f))
(test (interp (desugar (parse '{zero? 0})) (mtSub)) (boolV #t))
;(test (interp (desugar (parse '{throws DivisionEntreCero})) (mtSub)) (let/cc k (exceptionV 'DivisionEntreCero k)))
(test (interp (desugar (parse '{try/catch {{DivisionPorCero 2} {MiExcepcion 4}}
                                          {+ 1 {throws MiExcepcion}}})) (mtSub)) (numV 5))
