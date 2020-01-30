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
        (equal? f <)
        (equal? f <=)
        (equal? f mequal?)
        (equal? f not-equal?)
        (equal? f >)
        (equal? f >=)
        (equal? f not)
        (equal? f mand)
        (equal? f mor)))
        

;; Predicado para restringir el tipo de números.
(define (numero-valido? n)
    (or (integer? n) (real? n)))

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
    (not (mequal? args)))
                        

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


;; TDA para representar el árbol de sintaxis abstracto del lenguaje FWBAE.
(define-type FWBAE
    [idS    (i symbol?)]
    [numS   (n numero-valido?)]
    [boolS  (b boolean?)]
    [opS    (f operador-valido?) (args (listof FWBAE?))]
    [withS  (bindings (listof binding?)) (body FWBAE?)]
    [withS* (bindings (listof binding?)) (body FWBAE?)]
    [funS   (params (listof symbol?)) (body FWBAE?)]
    [appS   (fun-expr FWBAE?) (args (listof FWBAE?))])

;; TDA para asociar identificadores con valores.
(define-type Binding
    [binding (name symbol?) (value FWBAE?)])


;; TDA que es una versión sin azúcar sintáctica del TDA FWBAE.
(define-type FBAE
    [id   (i symbol?)]
    [num  (n numero-valido?)]
    [bool (b boolean?)]
    [op   (f operador-valido?) (args (listof FBAE?))]
    [fun  (param symbol?) (body FBAE?)]
    [app  (fun-expr FBAE?) (args (listof FBAE?))])

;; TDA para representar el ambiente de evaluación.
(define-type Env
    [mtSub]
    [aSub (name symbol?) (value FBAE-Value?) (env Env?)]
    [aRecSub (name symbol?) (value boxed-RCBAEL/L-Value?) (env Env?)])

;; TDA para representar los resultados devueltos por el intérprete.
(define-type FBAE-Value
    [numV     (n number?)]
    [boolV    (b boolean?)]
    [closureV (params symbol?) (body FBAE?) (env Env?)])
