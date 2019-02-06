#lang plai

#| Lenguajes de Programación
   Actividad de laboratorio 2 |#

;; Función que calcula el s-dígito de un número natural n.
;; sdigito: number -> number
(define (sdigito n)
    (if (< n 10)
        n
        (sdigito (+ (modulo n 10) (sdigito (quotient n 10))))))

(test (sdigito 19) 1)
(test (sdigito 345) 3)
(test (sdigito 23) 5)
(test (sdigito 454) 4)
(test (sdigito 357) 6)


;; Función auxiliar que cuenta los digitos un número.
;; cuenta-digitos: number -> number
(define (cuenta-digitos n)                       
    (if (< n 10)
        1
        (+ 1 (cuenta-digitos (quotient n 10)))))

;; Función auxiliar que invierte un número.
;; invierte-aux: number number -> number
(define (invierte-aux n len) 
    (if (< n 10)
       n
       (+ (* (modulo n 10) (expt 10 len)) (invierte-aux (quotient n 10) (- len 1)))))

;; Función que invierte un número.
;; invierte: number -> number
(define (invierte n) (invierte-aux n (- (cuenta-digitos n) 1)))

(test (invierte 12345) 54321)
(test (invierte 1) 1)
(test (invierte 123) 321)
(test (invierte 5790) 0975)
(test (invierte 45) 54)


;; Función que devuelve una lista con los primeros n elementos de la lista original.
;; toma: (listof a) number -> (listof a)
(define (toma lst n)
    (if (< n 1)
        empty
        (cons (car lst) (toma (cdr lst) (- n 1)))))

(test (toma (list 1 2 3 4) 2) (list 1 2))
(test (toma (list 1 2 3 4 5 6 7 8) 5) (list 1 2 3 4 5))
(test (toma (list 1 2 3 4) 1) (list 1))
(test (toma (list 1 2 3 4) 4) (list 1 2 3 4))
(test (toma (list 1 2 3 4) 0) empty)  

;; Función que devuelve una lista sin elementos antes de la posición n de la lista original.
;; quita: (listof a) number -> (listof a)
(define (quita lst n)
     (if (or (empty? lst) (< n 1))
         lst
         (quita (cdr lst) (- n 1))))
  
(test (quita (list 1 2 3) 1) (list 2 3))
(test (quita (list 1 2 3 4 5) 3) (list 4 5))
(test (quita (list 1 3) 0) (list 1 3))
(test (quita (list 1 2 3 5 6 2 1 7) 5) (list 2 1 7))
(test (quita (list 5 6 7 1 2 3) 3) (list 1 2 3))
