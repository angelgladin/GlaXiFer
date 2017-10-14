#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

;; Función que ejecuta el intérprete de FWAE con alcance estático.
;; static-FWAE: void
(define (FWBAE)
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
                    [else (display "#<function>")]))) 
            (display "\n")
            (FWBAE)))))

;; Línea que recibe los valores de la línea de comandos de acuerdo al lenguaje precedido del 
;; parámetro -i que indica qué interprete se ejecutará. Si no se ejecuta 
;; 
;; Parámetros:
;; FWAE/static - Ejecuta el intérprete de FWAE con alcance estático.
;; FWAE/dynamic - Ejecuta el intérprete de FWAE con alcance dinámico.
(define (ejecuta)
   (let ([args (current-command-line-arguments)])
      (cond
         [(zero? (vector-length args)) (FWBAE)]
         [(equal? (vector-ref args 0) "-i")
            (cond
               [(= (vector-length args) 2)
                  (match (vector-ref args 1)
                     ["FWBAE" (FWBAE)]
                     [else (error 'ejecuta "Language not defined")])]
               [else (error 'ejecuta "Language not specified")])])))

(ejecuta)
