#lang plai

(require "grammars.rkt")
(require "parser.rkt");;borrar al final

;; Desendulzador.
;; Regresa un árbol de sintaxis abstracta sin azúcar sintáctica.
;; desugar: CFWBAE/L -> CFBAE/L
(define (desugar expr)
  (match expr
    [(boolS i) (bool i)]
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(appS fun-expr args) (app (desugar fun-expr) (foldr (λ (v l) (cons (desugar v) l)) '() args))]
    [(ifS expr then-expr else-expr) (iF (desugar expr) (desugar then-expr) (desugar else-expr))]
    [(condS conditions) (match (car conditions)
                          [(condition expr then-expr) (desugar (ifS expr then-expr (condS (cdr conditions))))]
                          [(else-cond else-expr) (desugar else-expr)])]
    [(funS (cons x xs) body)
     (cond
       [(= 1 (length (cons x xs))) (fun x (desugar body))]
       [else (fun x (desugar (funS xs body))) ])]
    [(opS f args) (op f (map desugar args))]
    [(withS (cons x xs) body)
     (app
      (desugar (funS (foldr (λ (v l) (cons (binding-name v) l)) '() (cons x xs)) body))
      (foldr (λ (v l) (cons (desugar (binding-value v)) l)) '() (cons x xs)))]
    [(withS* f body) (cond
                       [(= 1 (length f)) (desugar(withS f body))]
                       [else (desugar (withS (list (car f)) (withS* (cdr f) body)))])]))
      
