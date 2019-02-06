#lang plai

(require "grammars.rkt")

;; Evalua expresiones del lenguaje Forth
;; evalua: Forth -> Pila
(define (evalua exp)
  (type-case Forth exp
    [fapp (op p) (let* ([f (evaluap (top p))][s (evaluap (top (pop p)))])
                   (consp (evaluap (push(op f s) (pop(pop p))))))]
    [fdrop (p) (consp (evaluap (pop p)))]
    [fdup (p) (let ([ a  (evaluap (top p))])
                (consp (evaluap (push a p))))]
    [fover (p) (consp (evaluap (push (* 2 (evaluap (top p))) p)))]
    [fswap (p)  (let* ([f (evaluap (top p))][s  (evaluap (top (pop p)))])
                  (consp (evaluap (push s (push f (pop (pop p)))))))]                  
    [fadd (e p) (consp (evaluap (push e p)))]))
  

;; Evalua expresiones de tipo Pila
;; evaluap: Pila -> (listof a)
;; Evalua expresiones de tipo Pila
;; evaluap: Pila -> (listof a)
(define (evaluap exp)
    (type-case Pila exp
      [consp (elems) elems]
<<<<<<< HEAD
      [push (e p) (push-p e (evaluap p))]
=======
      [push (e p) (cond
                    [(and (evaluap p) (not (any? e))) (error " no son del mismo tipo")]
                    [else (push-p e (evaluap p))])]
>>>>>>> dd70b063c47bb0bd73563d262bfbcddaca4ac418
      [pop (p) (pop-p (evaluap p))]
      [top (p) (car (evaluap p))]))



;; Auxiliar que evalua expresiones de tipo Pila en un push
;; evaluap: Pila number -> (listof a)
(define (push-p elemento pila)
  (match pila
    ['() (cons elemento empty)]
    [else (cons elemento pila)]))

;; Auxiliar que evalua expresiones de tipo Pila en un pop
;; pop-p: Pila  -> (listof a)
(define (pop-p pila)
   (match pila
      ['() '()]
<<<<<<< HEAD
      [(cons x xs)  xs]))
=======
      [(cons x xs)  xs])))
>>>>>>> dd70b063c47bb0bd73563d262bfbcddaca4ac418


(define p1 (consp '()))
(define p2 (evalua (fadd 2 p1)))
(define p3 (evalua (fadd 2 p2)))
(define p4 (evalua (fapp + p3)))
(define p5 (evalua (fdup p4)))
(define p6 (evalua (fadd 2 p5)))
(define p7 (evalua (fswap p6)))
(define p8 (evalua (fapp - p7)))
