#lang plai

;; tribonacci: number -> number
(define (tribonacci n)
    (cond
      [(= n 0) 0]
      [(= n 1) 0]
      [(= n 2) 1]
      [else (+ (tribonacci(- n 1)) (tribonacci(- n 2)) (tribonacci(- n 3)))])) 


;; tribonacci-cola: number -> number
(define (tribonacci-c n)
    (tribonacci-cola n 0 0 1)
  )
(define (tribonacci-cola n acc1 acc2 acc3)
  (cond
    [(= n 0) acc1]
    [(= n 1) acc2]
    [(= n 2) acc3]
    [else (tribonacci-cola (- n 1) acc2 acc3 (+ acc1 acc2 acc3))]))


;; tribonacci-memo: number -> number
(define tabla (make-hash (list (cons 0 0) (cons 1 0) (cons 2 1))))

(define (tribonacci-memo n)
  (let ([res (hash-ref tabla n 'ninguno) ]) 
  (cond
    [(equal? res 'ninguno)
         (hash-set! tabla n (+ (tribonacci-memo(- n 1)) (tribonacci-memo(- n 2)) (tribonacci-memo(- n 3))))
         (hash-ref tabla n)]
    [else res])))
