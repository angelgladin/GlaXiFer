#lang plai

;; tribonacci: number -> number
(define (tribonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 0]
    [(= n 2) 1]
    [else (+ (tribonacci(- n 1)) (tribonacci(- n 2)) (tribonacci(- n 3)))]))

;; tribonacci-cola: number -> number
(define (tribonacci-cola n)
  (tribonacci-cola-aux n 0 0 1))

;; Función auxiliar que es llamada por `tribonacci-cola`, se hace esto
;; para que ocultar al programador que se usará recursión de cola.
(define (tribonacci-cola-aux n acc1 acc2 acc3)
  (cond
    [(= n 0) acc1]
    [(= n 1) acc2]
    [(= n 2) acc3]
    [else (tribonacci-cola-aux (- n 1) acc2 acc3 (+ acc1 acc2 acc3))]))


;; Hash en el que definimos los casos base de tribonacci.
(define tabla (make-hash (list (cons 0 0)
                               (cons 1 0)
                               (cons 2 1))))

;; tribonacci-memo: number -> number
(define (tribonacci-memo n)
  (let ([res (hash-ref tabla n 'ninguno)])
    (cond
      [(equal? res 'ninguno)
       (hash-set! tabla n (+ (tribonacci-memo(- n 1)) (tribonacci-memo(- n 2))
                             (tribonacci-memo(- n 3))))
       (hash-ref tabla n)]
      [else res])))
