#lang plai

(require "grammars.rkt")

;; Analizador sintáctico.
;; Regresa el árbol de sintaxis abstracto asociado a la sintaxis concreta.
;; parse: s-expression -> WAE.
(define (parse sexp)
  (match sexp
    [(? symbol?)
     (cond
           [(equal? sexp 'false) (boolS #f)]
           [(equal? sexp 'true) (boolS #t)]
           [else (idS sexp)])]
    [(? number?) (numS sexp)]
    [(list 'with (cons x xs) body)
     (withS (foldr (lambda (v l) (cons (binding (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
    [(list 'with* (cons x xs) body)
     (withS* (foldr (lambda (v l) (cons (binding (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
    [(list 'fun param body)
      (funS param (parse body))]
    [(cons x xs)
     (if (operador-valido? (elige x))
          (opS (elige x) (foldr (lambda (v l) (cons (parse v) l)) '() xs))
          (appS (parse x) (map parse xs)))]))
  
;; Realiza un mapeo entre las operaciones del lenguaje anfitrión y el lenguaje objetivo.
;; elige: symbol -> procedure
(define (elige s)
   (match s
      ['+    +]
      ['-    -]
      ['*    *]
      ['/    /]
      ['%    mmodulo]
      ['min  min]
      ['max  max]
      ['pow  mexpt]
      ['sqrt sqrt]
      ['<    <]
      ['<=   <=]
      ['=    mequal?]
      ['/=   not-equal?]
      ['>    >]
      ['>=   >=]
      ['not  not]
      ['and  mand]
      ['or   mor]
      [else 'e]))

