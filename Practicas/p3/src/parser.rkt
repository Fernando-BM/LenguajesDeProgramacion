#lang plai

(require "grammars.rkt")

;; Analizador sintáctico.
;; Regresa el árbol de sintaxis abstracto asociado a la sintaxis concreta.
;; parse: s-expression -> WAE.
(define (parse sexp)
  (match sexp
    [(? symbol?) (id sexp)]
    [(? number?) (num sexp)]
    [(list 'with (cons x xs) body)
     (with (foldr (lambda (v l) (cons (binding (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
    [(list 'with* (cons x xs) body)
     (with* (foldr (lambda (v l) (cons (binding (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
    [(cons x xs)
     (op (elige x) (foldr (lambda (v l) (cons (parse v) l)) '() xs))]))
;; Realiza un mapeo entre las operaciones del lenguaje anfitrión y el lenguaje objetivo.
;; elige: symbol -> procedure
(define (elige s)
    (match s
        ['+ +]
        ['- -]
        ['* *]
        ['/ /]
        ['% mmodulo]
        ['min min]
        ['max max]
        ['pow mexpt]
        ['sqrt sqrt]))
