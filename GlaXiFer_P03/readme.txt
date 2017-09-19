╔════════════════════════════════════════════════════╗ 
║ Lenguajes de Programación                          ║ 
║ Práctica 3                                         ║
╚════════════════════════════════════════════════════╝

╔════════════════╗ 
║ Integrantes    ║ 
╚════════════════╝
Ángel Iván Gladín García
  313040131
  angelgladin@gmail.com
María Ximena Lezama Hernández
  313040131
  lezama@ciencias.unam.mx
María Fernanda González Chávez
  313036367
  fernandagch@ciencias.unam.mx

╔════════════════╗ 
║ Ejercicio 3.1  ║ 
╚════════════════╝
Expresiones aceptadas por la gramática del lenguaje WAE.
1. Identificadores válidos
   (id foo)
   (id baz)

2. Números válidos
   (num 666)
   (num -666)
   (num (/ 1 2))
   (num (sqrt 2))
   (num 1+2i)

3. Operaciones válidas
   (op + (list (num 1) (num 2) (num 3)))
   (op - (list (num 666) (num 666))
   (op * (list (num 111) (num 6)))
   (op / (list (num 1) (num 2) (num 2) (num 2)))
   (op modulo (list (num 666) (num 2)))
   (op min (list (num 666) (num 666) (num 666) (num 0)))
   (op max (list (num 666) (num 666) (num 666) (num 0)))
   (op * (list (num 1) (num 2) (num 2) (num 2)))
   (op expt (list (num 2) (num 2)))
   (op + (list (num 666) (op - (list (num 666) (num 666)))))

4. Expresiones con with válidas.
   (with (list (binding 'a (num 666))) (op + (list (num 666) (num 666))))
   (with (list (binding 'a (num 666))) (with (list (binding 'b (num 0))) (op + (list (id 'a) (id 'b)))))
   (with (list (binding 'a (num 666))) (with (list (binding 'b (id 'a))) (with (list (binding 'c (num 1))) (op + (list (id 'a) (id 'b) (id 'c))))))

5. Expresiones con with* válidas.
   (with* (list (binding 'a (num 0)) (binding 'b (id 'a))) (op + (list (id 'b) (id 'b))))
   (with* (list (binding 'a (num 0)) (binding 'b (num 1))) (op + (list (id 'a) (id 'b))))
   (with* (list (binding 'a (num 0)) (binding 'b (num 1)) (binding 'c (num 2))) (op + (list (id 'a) (id 'b) (id 'c))))

╔════════════════╗ 
║ Ejercicio 3.1  ║ 
╚════════════════╝
El analizador léxico recibe una expresión y las separa en lexemas. En Racket podemos ahorrarnos este análisis al usar una de sus primitivas ¿cuál es esta primitiva?, ¿cómo funciona?, ¿por qué es útil para realizar el análisis léxico?

La primitiva es Lexer. Lo que hace lexer es produce una función que toma un input-port, empareja los patrones “re” contra el búfer y devuelve el resultado de ejecutar la accion-expr correspondiente. Cuando varios patrones coinciden, un lexer elegirá el partido más largo, rompiendo los lazos en favor de la regla que aparece primero.
Es útil para el análisis léxico ya que solo guardamos las palabras, como hacer un trim, y emparejamos (comparamos) que estén bien estructurados.
.
\`*-.                    
)  _`-.                 
.  : `. .                
: _   '  \               
; *` _.   `*-._          
`-.-'          `-.       
;       `       `.     
:.       .        \    
. \  .   :   .-'   .   
'  `+.;  ;  '      :   
:  '  |    ;       ;-. 
; '   : :`-:     _.`* ;
*' /  .*' ; .*`- +'  `*' 
*-*   `*-*  `*-*'
