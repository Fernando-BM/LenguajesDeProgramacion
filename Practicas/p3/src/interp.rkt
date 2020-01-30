#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser.
;; interp: WAE -> number
(define (interp exp)
  (match exp
    [(num n) n]
    [(id v)  (error 'interp "Identificador libre")]
    [(op p body) (let [(int-body (map interp body))]
                   (cond
                     [(= 1 (length  int-body)) (p (car int-body))]
                     [(equal? p mmodulo) (mmodulo int-body)]
                     [(equal? p mexpt) (mexpt int-body)]
                     [else (mop int-body p)]))]
    [(with l bound-body)
     (interp (foldr (lambda (l e)
                      (subst e (binding-name l) (num (interp (binding-value l))))) bound-body l))]
    [(with* l bound-body)
     (interp (with (interp l) bound-body))]
    [(cons x xs) (cond
                   [(empty? xs) (cons x xs)]
                   [(cons x (interp (map (lambda (v) (binding (binding-name v)
                                                         (subst (binding-value v)
                                                                (binding-name x)
(binding-value x)))) xs)))])]))

;; Función que implementa el algoritmo de sustitución.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sub-id val)
	(match expr
          [(id i) (if (symbol=? i sub-id) val expr)]
          [(num n) expr]
          [(op f args) (op f (aux args sub-id val))]
          [(with bindings body)
                (if (contain bindings sub-id)
                    (with (subst-with bindings sub-id val) body)
                    (with (subst-with bindings sub-id val) (subst body sub-id val)))]
          [(with* bindings body)
                (if (contain bindings sub-id)
                    (with* (subst-with bindings sub-id val) body)
                    (with* (subst-with bindings sub-id val) (subst body sub-id val)))]
          ))

(define (subst-with bindings sub-id value)
  (match bindings
    ['() '()]
    [(cons (binding a b) xs) (cons (binding a (subst b sub-id value)) (subst-with xs sub-id value))]))

;; Función auxiliar para subst.
;; aux: list symbol WAE -> list
(define (aux lst sub-id val)
     (match lst
       ['() '()]
       [(cons x xs) (cons (subst x sub-id val) (aux xs sub-id val))]))

;; Función auxiliar para ver si un sub-id está contenido en
;; una lista de bindings.
;; contain: list symbol -> boolean
(define (contain lst sub-id)
  (match lst
    ['() false]
    [(cons (binding a b) xs) (if (symbol=? a sub-id) true (contain xs sub-id))]))

;; Función que substituye una lista de bindings a una expresión.
;; subst-lst: list WAE -> WAE.
(define (subst-lst bindings body)
  (match bindings
    ['() body]
    [(cons (binding var val) xs) (let [(res (subst body var val))] (subst-lst xs res))]))


;; Función que calcula el op de forma recursivo.
;; mmodulo: number number ... -> number
(define (mop args op)
  (letrec ([expt-lista (lambda (lst)
                         (if (= (length lst) 1)
                             (first lst)
                             (expt-lista (cons (op (first lst) (second lst)) (cdr (cdr lst))))))])   
 (if (and (= (length args) 1) (not(number? (car args)))) (expt-lista (car args)) (expt-lista args))))
