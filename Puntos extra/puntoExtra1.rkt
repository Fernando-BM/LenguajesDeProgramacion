#lang racket
#| 
Punto Extra
Bernal Martínez Fernando 31335230-4
Lenguajes de programación 2019-1

Actividad:
 Realizar la conversion de la lista creada con list a
 una lista creada con cons
|#

;;Lista creada apartir de list, con elementos mixtos
(list 4(list(list "adam" 0)(list "juan" 1)(list "luis" 2))3(list "daniel" 5)) 

;;Solución
;;Lista creada apartir de cons, con elementos mixtos
(cons 4
      (cons
        (cons
              (cons "adam"
                         (cons 0 empty))
              (cons
               (cons "juan"
                     (cons 1 empty))

               (cons
                (cons"luis"
                     (cons 2 empty)) empty)))
        (cons 3
              (cons
               (cons "daniel"
                     (cons 5 empty)) empty))))



