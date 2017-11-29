#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

;; Función que ejecuta el intérprete de ERCFWBAEL/L con alcance estático, evaluación perezosa y recursividad.
;; ERCFWBAEL/L: void
(define (ERCFWBAEL/L)
   (begin
      (display "(λ) ")
      (define x (read))
      (if (equal? x '{exit})
          (display "")
          (begin 
            (with-handlers ([exn:fail? (lambda (exn) (display "Error"))])
               (let ([result (interp (desugar (parse x)) (mtSub))])
                  (cond
                    [(numV? result) (display (numV-n result))]
                    [(boolV? result)
                      (if (boolV-b result)
                          (display "true")
                          (display "false"))]
                    [(listV? result) (display (listV->list (listV-elems result)))]
                    [(exceptionV? result) (display (exceptionV-exception-id result))]
                    [else (display "#<function>")]))) 
            (display "\n")
            (ERCFWBAEL/L)))))

;; Método auxiliar que dada una lista un listV lo pasa a una lista predeterminada de
;; Racket para que se vea bien cuando se imprime el valor
;; listV->list: listof ERCFBAEL/L-Value -> list
(define (listV->list l)
  (map (λ (v) (match v
                ; Extraemos el valor del constructor que tiene
                ; el tipo dato RCFBAEL/L-Value que regresa
                ; el intérprete.
                [(? numV?) (numV-n v)]
                [(? boolV?) (boolV-b v)]
                [(? listV?) (listV->list (listV-elems v))]
                [else v])) l))

;; Línea que recibe los valores de la línea de comandos de acuerdo al lenguaje precedido del 
;; parámetro -i que indica qué interprete se ejecutará. Si no se ejecuta 
;; 
;; Parámetros:
;; -- PENDIENTE
(define (ejecuta)
   (let ([args (current-command-line-arguments)])
      (cond
         [(zero? (vector-length args)) (ERCFWBAEL/L)]
         [(equal? (vector-ref args 0) "-i")
            (cond
               [(= (vector-length args) 2)
                  (match (vector-ref args 1)
                     ["RCFWBAEL/L" (ERCFWBAEL/L)]
                     [else (error 'ejecuta "Language not defined")])]
               [else (error 'ejecuta "Language not specified")])])))

(ejecuta)
