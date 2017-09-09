#lang plai

(require "parser.rkt")
(require "interp.rkt")

;; Función encargada de ejecutar el intérprete para que el usuario interactúe con el lenguaje. Para
;; diferenciar el prompt de Racket del nuestro, usamos "(λ)". Aprovechamos los aspectos imperativos
;; del lenguaje para esta función.
;; ejecuta: void
(define (ejecuta)
   (begin
      (display "(λ) ")
      (define x (read))
      (if (equal? x '{exit})
          (display "")
          (begin 
            (with-handlers ([exn:fail? (lambda (exn) (display "Ocurrió un error al interpretar"))]) 
               (display (interp (parse x))))
            (display "\n")
            (ejecuta)))))

;; Llamada a la función
(ejecuta)
