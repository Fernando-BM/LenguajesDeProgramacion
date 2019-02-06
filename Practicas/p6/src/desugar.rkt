#lang plai

(require "grammars.rkt")

;; Desendulzador.
;; Regresa un árbol de sintaxis abstracta sin azúcar sintáctica.
;; desugar: RCFWBAEL/L -> RCFBAEL/L
(define (desugar expr)
  (match expr
    [(boolS i) (bool i)]
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(listS elems) (lisT (map desugar elems))]
    [(appS fun-expr args) (app (desugar fun-expr) (map desugar args))]
    [(ifS expr then-expr else-expr) (iF (desugar expr) (desugar then-expr) (desugar else-expr))]
    [(condS conditions) (match (car conditions)
                          [(condition expr then-expr) (desugar (ifS expr then-expr (condS (cdr conditions))))]
                          [(else-cond else-expr) (desugar else-expr)])]
    [(funS (cons x xs) body)
     (cond
       [(= 1 (length (cons x xs))) (fun x (desugar body))]
       [else (fun x (desugar (funS xs body))) ])]
    [(opS f args) (op f (map desugar args))]
    [(recS bindings body) (rec (aux-parse-binding bindings) (desugar body))]
    [(withS (cons x xs) body)
     (app
      (desugar (funS (map bindingS-name (cons x xs)) body))
      (foldr (λ (v l) (cons (desugar (bindingS-value v)) l)) '() (cons x xs)))]
    [(withS* f body) (cond
                       [(= 1 (length f)) (desugar(withS f body))]
                       [else (desugar (withS (list (car f)) (withS* (cdr f) body)))])]))



;; Función auxiliar que hace un crea una lista de `bindings`.
;; aux-parse-bindings list list symbol -> list BindingS
(define (aux-parse-binding bindings)
  (map (λ (v) (binding (bindingS-name v) (desugar (bindingS-value v)))) bindings))