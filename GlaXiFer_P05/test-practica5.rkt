#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 5 |#

;; Pruebas para  parse

(test (parse 'true) (boolS #t))
(test (parse 'false) (boolS #f))
(test (parse '{if {< 2 3} 4 5}) (ifS (opS < (list (numS 2) (numS 3))) (num 4) (num 5)) )
