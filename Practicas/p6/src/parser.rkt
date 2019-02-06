#lang plai

(require "grammars.rkt")

;; Analizador sintáctico.
;; Regresa el árbol de sintaxis abstracto asociado a la sintaxis concreta.
;; parse: s-expression -> RCFWBAEL/L
(define (parse sexp)
  (match sexp
    [(? symbol?)
     (cond
       [(equal? sexp 'false) (boolS #f)]
       [(equal? sexp 'true) (boolS #t)]
       [(equal? sexp 'empty) (listS '())]
       [else (idS sexp)])]
    [(cons 'list elems) (listS (map parse elems))] 
    [(? number?) (numS sexp)]
    [(list 'if cond then-expr else-expr)
     (ifS  (parse cond) (parse then-expr) (parse else-expr))]
    [(cons 'cond conditions) 
     (condS (map parse-condiciones conditions))]
    [(list 'rec bindings body) 
     (recS (map (lambda (v) (bindingS (first v) (parse (second v)))) bindings) (parse body))]
    [(list 'with (cons x xs) body)
     (withS (foldr (lambda (v l) (cons (bindingS (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
    [(list 'with* (cons x xs) body)
     (withS* (foldr (lambda (v l) (cons (bindingS (first v) (parse (second v))) l)) '() (cons x xs)) (parse body))]
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
    ['inc  add1]
    ['dec  sub1]
    ['<    <]
    ['<=   <=]
    ['=    mequal?]
    ['/=   not-equal?]
    ['>    >]
    ['>=   >=]
    ['zero? zero?]
    ['not  not]
    ['and  mand]
    ['or   mor]
    ['head car]
    ['tail cdr]
    ['append append]
    ['empty? empty?]
    [else 'a]))

;;Auxiliar que parsea a condition cada condición
;;parse-condiciones: s-expression -> CFWBAE/L
(define (parse-condiciones conditions)
  (match conditions
    [(list 'else else-expr) (else-cond (parse else-expr))]
    [(list expr then-expr) (condition (parse expr) (parse then-expr))]))
