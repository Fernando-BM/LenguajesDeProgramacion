#lang plai


;; Tipo de dato abstracto para representar la sintaxis abstracta
;; del lenguaje Forth.
(define-type Forth
	[fapp  (op procedure?) (pila Pila?)]
	[fdrop (pila Pila?)]
	[fdup  (pila Pila?)]
	[fover (pila Pila?)]
	[fswap (pila Pila?)]
	[fadd (n number?) (pila Pila?)])

;; Predicado para permitir estructuras de cualqueir tipo.
;; any?: a -> bool
(define (any? a) true)

;; Tipo de dato abstracto para representar la sintaxis abstracta
;; de las operaciones asociadas a una pila.
(define-type Pila
	[consp (elems (listof any?))]
	[push  (e any?) (pila Pila?)]
	[pop   (pila Pila?)]
	[top   (pila Pila?)])

;; Evalua expresiones del lenguaje Forth
;; evalua: Forth -> Pila
(define (evalua exp)
  (type-case Forth exp
    [fapp (op p) (cond
                   [(equal? op +) (let* ([f (evaluap (top p))][s  (evaluap (top (pop p)))])
                                    (consp (evaluap (push(+ f s) (pop(pop p))))))]
                   [(equal? op -) (let* ([f (evaluap (top p))][s  (evaluap (top (pop p)))])
                                    (consp (evaluap (push(- f s) (pop(pop p))))))]
                   [(equal? op *) (let* ([f (evaluap (top p))][s  (evaluap (top (pop p)))])
                                    (consp (evaluap (push(* f s) (pop(pop p))))))]
                   [(equal? op /) (if (zero? (evaluap (top (pop p))))
                                      (error "zero-divition-error")
                                      (let* ([f (evaluap (top p))][s  (evaluap (top (pop p)))])
                                    (consp (evaluap (push(/ f s) (pop(pop p)))))))]
                   [else error "operación no válida"])]
    [fdrop (p) (consp (evaluap (pop p)))]
    [fdup (p) (let ([ a  (evaluap (top p))])
                (consp (evaluap (push a p))))]
    [fover (p) (consp (evaluap (push (* 2 (evaluap (top p))) p)))];;verificr lista vacia
    [fswap (p)  (let* ([f (evaluap (top p))][s  (evaluap (top (pop p)))])
                  (consp (evaluap (push s (push f (pop (pop p)))))))]                  
    [fadd (e p) (consp (evaluap (push e p)))]))
  

;; Evalua expresiones de tipo Pila
;; evaluap: Pila -> (listof a)
;; Evalua expresiones de tipo Pila
;; evaluap: Pila -> (listof a)
(define (evaluap exp)
    (type-case Pila exp
      [consp (elems) (cond 
                         [(not (equal? elems (filter number? elems))) (evaluap (push 1 (consp '(1 2 3 sdf))))(error "No todos los elementos de la pila son de tipo number")]
                         [elems])]
      [push (e p) (cond
                    [(and (evaluap p) (not (number? e))) (error " El valor no es de tipo number")]
                    [else (push-p e (evaluap p))])]
      [pop (p) (pop-p (evaluap p))]
      [top (p) (top-p (evaluap p))]))



;; Auxiliar que evalua expresiones de tipo Pila en un push
;; evaluap: Pila number -> (listof a)
(define (push-p elemento pila)
  (match pila
    ['() (cons elemento empty)]
    [else (append pila (cons elemento empty))]))

;; Auxiliar que evalua expresiones de tipo Pila en un pop
;; pop-p: Pila  -> (listof a)
(define (pop-p pila)
  (let ([pila (reverse pila)])
    (match pila
      ['() '()]
      [(cons x xs) (reverse xs)])))

;; Auxiliar que evalua expresiones de tipo Pila en un top
;; top-p: Pila  -> number
(define (top-p pila)
  (let ([pila (reverse pila)])
    (match pila
      ['() '()]
      [(cons x xs) x])))



(define p1 (consp '()))
(define p2 (evalua (fadd 2 p1)))
(define p3 (evalua (fadd 2 p2)))
(define p4 (evalua (fapp + p3)))
(define p5 (evalua (fdup p4)))
(define p6 (evalua (fadd 2 p5)))
(define p7 (evalua (fswap p6)))
(define p8 (evalua (fapp - p7)))
