#lang plai

#| Lenguajes de Programación
   Práctica 1 |#

;; Función que toma dos números enteros y los eleva a sí mismos para luego sumar las potencias, es
;; decir, debe regresar a^b + b^a.
;; pot-sum: number number -> number
(define (pot-sum a b)
    (+(expt a b)(expt b a)))  ;;Utilize expt, la cual eleva a n un número

;; Función que calcula el área de un triángulo dados sus lados, usando la fórmula de Herón.
;; area-heron: number number number -> number
(define (area-heron a b c)
  (let ([s (/ (+ (+ a  b)c) 2)])
    
    (sqrt (* s (*(* (- s c ) (- s a))(- s b))))
  )
)

;; Función que dado un día de la semana representado mediante los 0 = do, 1 = lu, 2 = ma, ... sá = 6,
;; y un valor booleano y un valor booleano indicando si son vacaciones, regresa un símbolo que indica
;; a qué hora sonará una alarma.
;; alarma: number boolean -> symbol
(define (alarma dia vacaciones)
  (cond [(equal? vacaciones #f)(cond
                [(= dia 0) '7:00]
                [(= dia 6) '7:00]
                [else '10:00] ) ]
        [else (cond
                [(= dia 0) 'apagado]
                [(= dia 6) 'apagado]
                [else '10:00] ) ]
))

;; Predicado que dados dos números enteros, determina si el segundo es divisor propio del primero.
;; divisor-propio?: number number -> boolean
(define (divisor-propio? n m)
  (cond
    [(=(modulo n m) 0) ;;Utilize modulo, la cual calcula el modulo de un número 
     (cond
      [(= n m) #f]
      [else #t ])]
    [else #f]
    )
  )


;;Predicado que devuelve las suma de los m divisores de un número n
;;Recibe un n número y aux posible divisor del número n
;; suma-divisores: number -> number -> number
(define (calcula-divisores n aux)
  (define suma 0) ;variable auxiliar que lleva la suma de los m numeros divisibles de n
  
  (cond
    [(= n aux) suma] ;;Cuando n es igual a auxiliar, significa que ya no hay mas divisores y retornamos la suma
    [(equal? (divisor-propio? n aux) #t)                                     
                                       (let
                                           ([suma (+ aux (calcula-divisores n (+ 1 aux)))]) ;;A a suma le asignamos los m divisores
                                         suma) ] ;;solo 'regresamos' la suma
    [else (calcula-divisores n (+ 1 aux))] ;;'iterador' que nos ayuda a ir conociedo los n+1 numeros divisores
    )
 )



;; Predicado que dado un número natural, determina si es un número perfecto.
;; es-perfecto?: number -> boolean
(define (es-perfecto? n)
    (cond
     [(= n (calcula-divisores n 1)) #t]
     [else #f]
  )
  )



;; Predicado que dado dos números naturales, determina si son números amigos.
;; son-amigos?: number -> number ->boolean

(define (son-amigos? a b)
  
  (let* (
         [suma (calcula-divisores a 1)]
         [sumb (calcula-divisores b 1)])
    (if (= suma b) (if (= sumb a) #t #f) #f)
    
    )
  )







;; Función auxiliar que calcula el cuadrado s-dígito de un número natural n.
;; sdigito: number -> number
(define (sdigito n m)
  (cond
  [(< m 20)  
  (if (< n 10)
        n 
        (sdigito (+ (expt (modulo n 10) 2 )
                    (expt (sdigito (quotient n 10)(+ m 1)) 2)) (+ m 1)))]
  [else -1])
  )

;; Predicado que dado un número natural, determina si un número es feliz.
;; es-feliz?: number -> boolean
(define (es-feliz? n)
    (cond
      [(< n 10) (if (= 1 (sdigito (expt n 2) 0))
                    #t
                    #f)
                    ]
      [else (if (= 1 (sdigito n 0))
                #t
                #f)]))





;; Función recursiva que encuentra los números primos en un rango de m a n usando la Criba de 
;; Eratóstenes.
;; criba-eratostenes number number -> (listof number)
(define (criba-eratostenes n)
    #| Aquí va su código. |#)

;; Función recursiva que toma un número y regresa una lista de pares con la descomposición en primos
;; del mismo.
;; descomposicion-primos: number -> (listof number)
(define (descomposicion-primos n)
    #| Aquí va su código. |#)

;; Función que recibe una lista de números entre 0 y 99 y regresa una lista con su representación en
;; japones.
;; a-japones: (listof number) -> (listof string)
(define (a-japones lista)
    #| Aquí va su código. |#)

;; Función que recibe una lista de números y regresa una nueva lista que contiene únicamente aquellos
;; que son felices.
(define (felices lista)
    #| Aquí va su código. |#)

;; Función que encuentra el factorión de un número.
;; factorionr: number -> number
(define (factorionr n)
    #| Aquí va su código. |#)

;; Función que encuentra el factorión de un número.
;; factorionl: number -> number
(define (factorionl n)
#| Aquí va su código. |#)
