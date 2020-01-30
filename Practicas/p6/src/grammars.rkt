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
      (equal? f sqrt)
      (equal? f add1)
      (equal? f sub1)
      (equal? f <)
      (equal? f <=)
      (equal? f mequal?)
      (equal? f not-equal?)
      (equal? f >)
      (equal? f >=)
      (equal? f zero?)
      (equal? f not)
      (equal? f mand)
      (equal? f mor)
      (equal? f car)
      (equal? f cdr)
      (equal? f append)
      (equal? f empty?)))

;; Predicado para restringir el tipo de números.
(define (numero-valido? n)
  (or (integer? n) (real? n)))

;; Predicado para trabajar con cajas que guarden el resultado de evaluación.
(define (boxed-RCBAEL/L-Value? v)
  (and (box? v) (RCFBAEL/L-Value? (unbox v))))

;; Función que calcula el módulo de forma multiparamétrica.
;; mmodulo: number number ... -> number
(define (mmodulo . args)
  (letrec ([expt-lista (lambda (lst)
                         (if (= (length lst) 1)
                             (first lst)
                             (expt-lista (cons (modulo (first lst) (second lst)) (cdr (cdr lst))))))])   
    (if (and (= (length args) 1) (not(number? (car args)))) (expt-lista (car args)) (expt-lista args))))

;; Función que calcula la potencia de forma multiparamétrica.
;; mmexpt: number number ... -> number
(define (mexpt . args)
  (letrec ([expt-lista (lambda (lst)
                         (if (= (length lst) 1)
                             (first lst)
                             (expt-lista (cons (expt (first lst) (second lst)) (cdr (cdr lst))))))])   
    (if (and (= (length args) 1) (not(number? (car args)))) (expt-lista (car args)) (expt-lista args))))


;; Función que indica si las expresiones pasadas como parámetro son iguales.
;; mequal?: any any ... -> boolean
(define (mequal? . args)
  (letrec ([eq-lista (lambda (lst arg)
                       (cond
                         [(= (length arg) 1) #t]
                         [(= (length arg) 2) (first(cons (equal? (first arg) (second arg)) lst))]
                         [else (eq-lista (list (equal? (first arg) (second arg))) (cdr arg))]))])
    (eq-lista '() args)))



;; Función que indica si las expresiones pasadas como parámetro son distintas.
;; not-equal?: any any ... -> boolean
(define (not-equal? . args)
  (letrec ([eq-lista (lambda (lst arg)
                       (cond
                         [(= (length arg) 1) #t]
                         [(= (length arg) 2) (first(cons (equal? (first arg) (second arg)) lst))]
                         [else (eq-lista (list (equal? (first arg) (second arg))) (cdr arg))]))])
    (not (eq-lista '() args))))
                        

;; Función que calcula la conjunción de forma multiparamétrica.
;; mand: boolean boolean ... -> boolean
(define (mand . args)
  (letrec ([expt-lista (lambda (lst)
                         (cond
                           [(equal? lst empty) #t]
                           [(= (length (list lst)) 1) (first lst)]
                           [else (expt-lista (cons (and (first lst) (second lst)) (cdr (cdr lst))))]))])
    (expt-lista args)))   

;; Función que calcula la disyunción de forma multiparamétrica.
;; mor: boolean boolean ... -> boolean
(define (mor . args)
  (letrec ([expt-lista (lambda (lst)
                         (cond
                           [(equal? lst empty) #f]
                           [(= (length (list lst)) 1) (first lst)]
                           [else (expt-lista (cons (or (first lst) (second lst)) (cdr (cdr lst))))]))])
    (expt-lista args)))  

;; TDA para representar el árbol de sintaxis abstracto del lenguaje CFWBAE/L.
(define-type RCFWBAEL/L
  [idS    (i symbol?)]
  [numS   (n numero-valido?)]
  [boolS  (b boolean?)]
  [listS  (elems (listof RCFWBAEL/L?))]
  [opS    (f operador-valido?) (args (listof RCFWBAEL/L?))]
  [ifS    (test-expr RCFWBAEL/L?) (then-expr RCFWBAEL/L?) (else-expr RCFWBAEL/L?)]
  [condS  (cases (listof Condition?))]
  [withS  (bindings (listof bindingS?)) (body RCFWBAEL/L?)]
  [withS* (bindings (listof bindingS?)) (body RCFWBAEL/L?)]
  [recS   (bindings (listof bindingS?)) (body RCFWBAEL/L?)]
  [funS   (params (listof symbol?)) (body RCFWBAEL/L?)]
  [appS   (fun-expr RCFWBAEL/L?) (args (listof RCFWBAEL/L?))])

;; TDA para asociar identificadores con valores con azúcar sintáctica.
(define-type BindingS
  [bindingS (name symbol?) (value RCFWBAEL/L?)])

;; TDA para asociar identificadores con valores sin azúcar sintáctica.
(define-type Binding
  [binding (name symbol?) (value RCFBAEL/L?)])

;; TDA que es una versión sin azúcar sintáctica del TDA CFWBAE/L.
(define-type RCFBAEL/L
  [id   (i symbol?)]
  [num  (n numero-valido?)]
  [bool (b boolean?)]
  [lisT (elems (listof RCFBAEL/L?))]
  [op   (f operador-valido?) (args (listof RCFBAEL/L?))]
  [iF   (test-expr RCFBAEL/L?) (then-expr RCFBAEL/L?) (else-expr RCFBAEL/L?)]
  [fun  (param symbol?) (body RCFBAEL/L?)]
  [rec  (bindings (listof Binding?)) (body RCFBAEL/L?)]
  [app  (fun-expr RCFBAEL/L?) (args (listof RCFBAEL/L?))])


(define-type Env
  [mtSub]
  [aSub    (name symbol?) (value RCFBAEL/L-Value?) (env Env?)]
  [aRecSub (name symbol?) (value boxed-RCBAEL/L-Value?) (env Env?)])

;; TDA para representar los resultados devueltos por el intérprete.
(define-type RCFBAEL/L-Value
  [numV     (n number?)]
  [boolV    (b boolean?)]
  [closureV (param symbol?) (body RCFBAEL/L?) (env Env?) (tbl hash?)]
  [exprV    (expr RCFBAEL/L?) (env Env?)]
  [listV    (elems (listof RCFBAEL/L-Value?))])

;; TDA para representar condiciones.
(define-type Condition
  [condition (test-expr RCFWBAEL/L?) (then-expr RCFWBAEL/L?)]
  [else-cond (else-expr RCFWBAEL/L?)])

