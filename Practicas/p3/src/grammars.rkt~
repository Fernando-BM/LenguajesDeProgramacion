#lang plai

;; Predicado que permite validar los operadores.
(define (operador-valido? f)
    (or (equal? f +) 
        (equal? f -)
        (equal? f *) 
        (equal? f /) 
        (equal? f mmodulo)
        (equal? f min)
        (equal? f max) 
        (equal? f mexpt)
        (equal? f sqrt)))

;; Predicado para restringir el tipo de números.
(define (numero-valido? n)
    (or (integer? n) (real? n)))

;; Función que calcula el módulo de forma multiparamétrica.
;; mmodulo: number number ... -> number
(define (mmodulo . args)
    #| Aquí va su código. |#)

;; Función que calcula el módulo de forma multiparamétrica.
;; mmexpt: number number ... -> number
(define (mexpt . args)
    #| Aqui va su código. |#)

;; TDA para representar el árbol de sintaxis abstracto del lenguaje WAE.
(define-type WAE
   [id    (i symbol?)]
   [num   (n numero-valido?)]
   [op    (f operador-valido?) (args (listof WAE?))]
   [with  (bindings (listof binding?)) (body WAE?)]
   [with* (bindings (listof binding?)) (body WAE?)])

;; TDA para asociar identificadores con valores.
(define-type Binding
   [binding (name symbol?) (value WAE?)])
