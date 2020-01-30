#lang plai

;; Map con recursividad de cola.
;; mapeo: (-> a b) '(a) -> '(b)
(define (mapeo f lst)
  (mapeo/cola f lst '()))

(define (mapeo/cola f l acc)
  (if(empty? l)
     acc
     (cons (f (car l)) (mapeo/cola f (cdr l) acc))))

;; Filter con recursividad de cola.
;; filtro: (-> a boolean) '(a) -> '(a)
(define (filtro p lst)
  (filtro/cola p lst '()))

(define (filtro/cola p l acc)
  (if(empty? l)
     acc
     (if (p (car l))
         (cons (car l) (filtro/cola p (cdr l) acc))
         (filtro/cola p (cdr l) acc))))

;; Foldr con recursiviad de cola.
;; plieguer: (-> a b b) b '(a) -> b
(define (plieguer f v lst)
  (foldrm/cola f v (reverse lst) v))

;; Foldl con recursiviad de cola.
;; plieguel: (-> a b b) b '(a) -> b
(define (plieguel f v lst)
  (foldrm/cola f v lst v))

;;foldrm/cola:(-> a b b) b '(a) -> b
(define (foldrm/cola f v lst acc)
  (match lst
    ['() acc]
    [(cons x xs) (foldrm/cola f v xs (f x acc))]))