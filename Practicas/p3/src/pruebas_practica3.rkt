#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)
(plai-ignore-exn-strings #t)
(test-inexact-epsilon 2)

#| Módulo de pruebas unitarias para la Práctica 3 |#

;; Expresiones de prueba.

(define expr01 'foo)
(define expr02 '1729)
(define expr03 '12.46)
(define expr04 
	'{+ {- 1 2 3} 
	    {* 4 5 6} 
	    {/ 20 2 2} 
	    {% 20 2 3} 
	    {min 1 7 2 9} 
	    {max 1 8 3 5} 
	    {pow 2 3 4} 
	    {sqrt 81}})
(define expr05
	'{with {{a 2} {b 3} {c 4}}
		{+ a b c}})
(define expr06
	'{with {{a {+ 2 3}} {b {* 7 4}} {c {max 1 7 2}}}
		{sqrt {% a b c}}})
(define expr07
	'{with {{a 2} {b 3} {c {+ 2 3}} {d {with {{e 1}} e}}}
		{max a b c d}})
(define expr08
	'{with* {{a 2} {b {+ a a}}}
		b})
(define expr09
	'{with* {{a 2} {b {+ a a}} {c {* a b}}}
		{with* {{d c} {e d}}
			{+ a b c d e}}})
(define expr10
	'{with* {{a x} {b {+ a a}}}
		{with {{x b}}
			x}})

;; Pruebas para el ejercicio 3.1

(test (mmodulo 0) 0)
(test (mmodulo 10 2) 0)
(test (mmodulo 10 3) 1)
(test (mmodulo 1729 10 3) 0)
(test (mmodulo 1729 10 4) 1)

(test (mexpt 0) 0)
(test (mexpt 2 3 4) 4096)
(test (mexpt 2 3 4 5) 1152921504606846976)
(test (mexpt 0 2 3 4 5) 0)
(test (mexpt 1 2 3 4 5) 1)

(test (parse expr01) (id 'foo))
(test (parse expr02) (num 1729))
(test (parse expr03) (num 12.46))
(test (parse expr04) 
	(op + 
		(list (op - (list (num 1) (num 2) (num 3))) 
			  (op * (list (num 4) (num 5) (num 6))) 
			  (op / (list (num 20) (num 2) (num 2)))
			  (op mmodulo (list (num 20) (num 2) (num 3)))
			  (op min (list (num 1) (num 7) (num 2) (num 9)))
			  (op max (list (num 1) (num 8) (num 3) (num 5)))
			  (op mexpt (list (num 2) (num 3) (num 4)))
			  (op sqrt (list (num 81))))))
(test (parse expr05)
	(with (list (binding 'a (num 2)) (binding 'b (num 3)) (binding 'c (num 4))) 
		  (op + (list (id 'a) (id 'b) (id 'c)))))
(test (parse expr06)
	(with 
		(list 
			(binding 'a (op + (list (num 2) (num 3)))) 
			(binding 'b (op * (list (num 7) (num 4)))) 
			(binding 'c (op max (list (num 1) (num 7) (num 2)))))
		(op sqrt (list (op mmodulo (list (id 'a) (id 'b) (id 'c)))))))
(test (parse expr07)
	(with
		(list
			(binding 'a (num 2))
			(binding 'b (num 3))
			(binding 'c (op + (list (num 2) (num 3))))
			(binding 'd (with (list (binding 'e (num 1))) (id 'e))))
		(op max (list (id 'a) (id 'b) (id 'c) (id 'd)))))
(test (parse expr08)
	(with*
		(list
			(binding 'a (num 2))
			(binding 'b (op + (list (id 'a) (id 'a)))))
		(id 'b)))
(test (parse expr09)
	(with*
		(list
			(binding 'a (num 2))
			(binding 'b (op + (list (id 'a) (id 'a))))
			(binding 'c (op * (list (id 'a) (id 'b)))))
		(with*
			(list
				(binding 'd (id 'c))
				(binding 'e (id 'd)))
			(op + (list (id 'a) (id 'b) (id 'c) (id 'd) (id 'e))))))
(test (parse expr10)
	(with*
		(list
			(binding 'a (id 'x))
			(binding 'b (op + (list (id 'a) (id 'a)))))
		(with
			(list
				(binding 'x (id 'b)))
			(id 'x))))

;; Pruebas para el ejercicio 3.2

(test (subst (parse expr01) 'x (num 1729)) (parse expr01))
(test (subst (parse expr02) 'x (num 1729)) (parse expr02))
(test (subst (parse expr03) 'x (num 1729)) (parse expr03))
(test (subst (parse expr04) 'x (num 1729)) (parse expr04))
(test (subst (parse expr05) 'x (num 1729)) (parse expr05))
(test (subst (parse expr06) 'x (num 1729)) (parse expr06))
(test (subst (parse expr07) 'x (num 1729)) (parse expr07))
(test (subst (parse expr08) 'x (num 1729)) (parse expr08))
(test (subst (parse expr09) 'x (num 1729)) (parse expr09))
(test (subst (parse expr09) 'x (num 1729)) (parse expr09))
(test (subst (parse expr10) 'x (num 1729))
	(with*
		(list
			(binding 'a (num 1729))
			(binding 'b (op + (list (id 'a) (id 'a)))))
		(with
			(list
				(binding 'x (id 'b)))
			(id 'x)))) 

;; Pruebas para el ejercicio 3.3

(test/exn (interp (parse expr01)) "Identificador libre")
(test (interp (parse expr02)) 1729)
(test (interp (parse expr03)) 12.46)
(test (interp (parse expr04)) 4235)
(test (interp (parse expr05)) 9)
(test (interp (parse expr06)) 2.23)
(test (interp (parse expr07)) 5)
(test (interp (parse expr08)) 4)
(test (interp (parse expr09)) 30)
(test/exn (interp (parse expr10)) "Identificador libre")