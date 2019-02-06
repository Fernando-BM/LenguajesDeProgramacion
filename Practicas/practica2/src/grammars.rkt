#lang plai

;; Tipo de dato abstracto para representar la sintaxis abstracta
;; del lenguaje Forth.
(define-type Forth
	[fapp  (op procedure?) (pila Pila?)]
	[fdrop (pila Pila?)]
	[fdup  (pila Pila?)]
	[fover (pila Pila?)]
	[fswap (pila Pila?)]
	[fadd (n number?) (pila Pila?)])

;; Predicado para permitir estructuras de cualqueir tipo.
;; any?: a -> bool
(define (any? a) true)

;; Tipo de dato abstracto para representar la sintaxis abstracta
;; de las operaciones asociadas a una pila.
(define-type Pila
	[consp (elems (listof any?))]
	[push  (e any?) (pila Pila?)]
	[pop   (pila Pila?)]
	[top   (pila Pila?)])
