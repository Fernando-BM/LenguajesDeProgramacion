#lang plai

;; Función que toma dos números enteros y los eleva a sí mismos para luego sumar las potencias, es
;; decir, debe regresar a^b + b^a.
;; pot-sum: number number -> number
(define (pot-sum a b)
    (+(expt a b)(expt b a)))  ;;Utilize expt, la cual eleva a n un número

(test (pot-sum 2 3) 17)
(test (pot-sum 8 9) 177264449)
(test (pot-sum 5 2) 57)
(test (pot-sum 13 34) 74829695578286078094567232718710148393)
(test (pot-sum 2 2) 8)


;; Función que calcula el área de un triángulo dados sus lados, usando la fórmula de Herón.
;; area-heron: number number number -> number
(define (area-heron a b c)
  (let ([s (/ (+ (+ a  b)c) 2)])
    (sqrt (* s (*(* (- s c ) (- s a))(- s b))))
  )
)

(test (area-heron 3 25 26) 36)
(test (area-heron 11 12 12) 58.65950477117924)
(test (area-heron 13 12 9) 52.15361924162119)
(test (area-heron 10 20 20) 96.82458365518542)
(test (area-heron 30 40 45) 588.1313097429859)


;; Función que dado un día de la semana representado mediante los 0 = do, 1 = lu, 2 = ma, ... sá = 6,
;; y un valor booleano y un valor booleano indicando si son vacaciones, regresa un símbolo que indica
;; a qué hora sonará una alarma.
;; alarma: number boolean -> symbol
(define (alarma dia vacaciones)
    (if (equal? vacaciones #t)

        (cond
         [(= dia 1) '10:00]
         [(= dia 2) '10:00]
         [(= dia 3) '10:00]
         [(= dia 4) '10:00]
         [(= dia 5) '10:00]
         [(= dia 6) 'apagada]
         [(= dia 0) 'apagada]
         [else (error 'dia-incorrecto)])

    (cond
         [(= dia 1) '7:00]
         [(= dia 2) '7:00]
         [(= dia 3) '7:00]
         [(= dia 4) '7:00]
         [(= dia 5) '7:00]
         [(= dia 6) '10:00]
         [(= dia 0) '10:00]
         [else (error 'dia-incorrecto)])))


(test (alarma 1 #f) '7:00)
(test (alarma 2 #f) '7:00)
(test (alarma 3 #t) '10:00)
(test (alarma 0 #t) 'apagada)
(test (alarma 6 #t) 'apagada)

;; Predicado que dados dos números enteros, determina si el segundo es divisor propio del primero.
;; divisor-propio?: number number -> boolean
(define (divisor-propio? n m)
  (cond
    [(=(modulo n m) 0) ;;Utilize modulo, la cual calcula el modulo de un número 
     (cond
      [(= n m) #f]
      [else #t ])]
    [else #f]
    ))

(test (divisor-propio? 28 7) #t)
(test (divisor-propio? 1234 1234) #f)
(test (divisor-propio? 12 6) #t)
(test (divisor-propio? 44184 789) #t)
(test (divisor-propio? 58788 852) #t)


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
       suma)] ;;solo 'regresamos' la suma
    [else (calcula-divisores n (+ 1 aux))] ;;'iterador' que nos ayuda a ir conociedo los n+1 numeros divisores
    ))

;; Predicado que dado un número natural, determina si es un número perfecto.
;; es-perfecto?: number -> boolean
(define (es-perfecto? n)
    (cond
     [(= n (calcula-divisores n 1)) #t]
     [else #f]))

(test (es-perfecto? 6) #t)
(test (es-perfecto? 28) #t)
(test (es-perfecto? 496) #t)
(test (es-perfecto? 8128) #t)
(test (es-perfecto? 33550336) #t)
(test (es-perfecto? 683837) #f)

;; Predicado que dado dos números naturales, determina si son números amigos.
;; son-amigos?: number -> number ->boolean

(define (son-amigos? a b)
  (let* (
         [suma (calcula-divisores a 1)]
         [sumb (calcula-divisores b 1)])
    (if (= suma b) (if (= sumb a) #t #f) #f)
    ))

(test (son-amigos? 220 284) #t)
(test (son-amigos? 1184 1210) #t)
(test (son-amigos? 5020 5564) #t)
(test (son-amigos? 10744 10856) #t)
(test (son-amigos? 184 110) #f)


;; Función auxiliar que calcula el cuadrado s-dígito de un número natural n.
;; sdigito: number -> number
(define (sdigito n m)
  (cond
  [(< m 20)  
  (if (< n 10)
        n 
        (sdigito (+ (expt (modulo n 10) 2 )
                    (expt (sdigito (quotient n 10)(+ m 1)) 2)) (+ m 1)))]
  [else -1]))

;; Predicado que dado un número natural, determina si un número es feliz.
;; es-feliz?: number -> boolean
(define (es-feliz? n)
    (cond
      [(< n 10) (if (= 1 (sdigito (expt n 2) 0)) #t #f)]
      [else (if (= 1 (sdigito n 0)) #t #f)]))


(test (es-feliz? 7) #t)
(test (es-feliz? 19) #t)
(test (es-feliz? 97) #t)
(test (es-feliz? 133) #t)
(test (es-feliz? 139) #t)
(test (es-feliz? 11) #f)


  
;; Función Auxiliar recursiva que regresa sin los multiplos de n
;; elimina-tablas (listof number) number number -> (listof number)
(define (elimina-tablas lista n meta)
    (match lista
      ['() '()]
      [(cons x xs)
     
     (if(= (modulo x n) 0)
        (elimina-tablas xs n meta)
        (cons x (elimina-tablas xs n meta)) 
        )]))

;; Función Auxiliar recursiva que itera las lista y elimina los multiplos de n+1 
;; (elimina-rec (listof number) number number -> (listof number)
(define (elimina-rec lista n meta)
  (if(<= n 10)
     (elimina-rec(elimina-tablas lista n meta) (+ n 1) meta)
     lista
     ))


;; Función Auxiliar recursiva que regresa la lista en un rango [m n]
;; mi-lista number number -> (listof number)
(define (mi-lista m n)
  (cond
    [(= n m)(cons n empty)]
    [else (cons m (mi-lista (+ 1 m) n))]
   ))

;; Función recursiva que encuentra los números primos en un rango de m a n usando la Criba de 
;; Eratóstenes.
;; criba-eratostenes number number -> (listof number)
(define (criba-eratostenes n)
  (cond
    [(>= n 7) (cons 2 (cons 3 (cons 5 (cons 7 (elimina-rec (mi-lista 2 n) 2 n)))))]
    [(>= n 5) '(2 3 5)]
    [(>= n 3) '(2 3)]
    [else (if (>= n 2)'(2) '()) ])   
)


(test (criba-eratostenes 20) '(2 3 5 7 11 13 17 19))
(test (criba-eratostenes 30) '(2 3 5 7 11 13 17 19 23 29))
(test (criba-eratostenes 70) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67))
(test (criba-eratostenes 70) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67))
(test (criba-eratostenes 100) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67
                                71 73 79 83 89 97))

;;Función aux recursiva que toma un número y una lista y regresa lista de pares con la descomposición en primos
;; del mismo.
;; div-primos: number -> (listof number)

(define (div-primos n lista contador)
  (if (equal? lista '())
    empty
    (if(= (modulo n (car lista)) 0)
       (cons (cons (car lista) (+ 1 contador)) (div-primos (quotient n (car lista)) lista (+ 1 contador)))
       (div-primos n (cdr lista) (* contador 0)))))


;;Función aux recursiva que toma un un lista y elimina tuplas repetidas
;; del mismo.
;; div-primos: (listof number) -> (listof number)
(define (elimina-rep lista)
  (if (equal? lista (cons (car lista) empty)) ;;Si la lista es igual a su cabeza, es decir, que es de un elemento entonces regresamos el elemento
      (cons (car lista) empty)
   (if (= (caar lista) (caadr lista))  (elimina-rep (cdr lista)) (cons (car lista) (elimina-rep (cdr lista) )) )

   )
  )
;; Función recursiva que toma un número y regresa una lista de pares con la descomposición en primos
;; del mismo.
;; descomposicion-primos: number -> (listof number)
(define (descomposicion-primos n)
  (elimina-rep (div-primos n (criba-eratostenes n) 0))
  )

(test (descomposicion-primos 616) '((2 . 3) (7 . 1) (11 . 1)))
(test(descomposicion-primos 529) '((23 . 2)))
(test(descomposicion-primos 554) '((2 . 1) (277 . 1)))
(test(descomposicion-primos 389) '((389 . 1)))
(test(descomposicion-primos 1303) '((1303 . 1)))



;; Función que recibe una lista de números entre 0 y 99 y regresa una lista con su representación en
;; japones.
;; a-japones: (listof number) -> (listof string)
(define (a-japones lista)
  (letrec([chinos (lambda (numero)
  (cond
    [(= numero 0) "rei"]
    [(= numero 1) "ichi"]
    [(= numero 2) "ni"]
    [(= numero 3) "san"]
    [(= numero 4) "yon"]
    [(= numero 5) "go"]
    [(= numero 6) "roku"]
    [(= numero 7) "nana"]
    [(= numero 8) "haci"]
    [(= numero 9) "kyu"]
    [(= numero 10)"ju"]
    [else (if(= 0 (modulo numero 10 ))
      (string-append (chinos (quotient numero 10 )) " ju" )
      (string-append (chinos (quotient numero 10 )) " ju " (chinos (modulo numero 10 ))))
       ]
   ))])
      
      (map chinos lista)))

(test (a-japones '(20 37 83)) '("ni ju" "san ju nana" "haci ju san"))
(test (a-japones '(15 13 11)) '("ichi ju go" "ichi ju san" "ichi ju ichi"))
(test (a-japones '(27 30 81)) '("ni ju nana" "san ju" "haci ju ichi"))
(test (a-japones '(32 47 93)) '("san ju ni" "yon ju nana" "kyu ju san"))
(test (a-japones '(25 6 18 21 90)) '("ni ju go" "roku" "ichi ju haci" "ni ju ichi" "kyu ju"))

;; Función que recibe una lista de números y regresa una nueva lista que contiene únicamente aquellos
;; que son felices.
(define (felices lista)
  (letrec
      ([sum-digito (lambda (n m)
        (cond
          [(< m 20)  
           (if (< n 10)
               n 
               (sum-digito (+ (expt (modulo n 10) 2 )
                           (expt (sum-digito (quotient n 10)(+ m 1)) 2)) (+ m 1)))]
          [else -1]))]
       [es-feliz-rec? (lambda (n)
                    (cond
                      [(< n 10) (if (= 1 (sum-digito (expt n 2) 0)) #t #f)]
                      [else (if (= 1 (sum-digito n 0)) #t #f)]))])
    (filter es-feliz-rec? lista)))

(test (felices '(1 2 3 4 5 6 7)) '(1 7))
(test (felices '(1 2 3 4 5 6 7 8 9 10 11)) '(1 7 10))
(test (felices '(1 2 3 4 5 6 7 12 13 14)) '(1 7 13))
(test (felices '(1 2 3 4 5 6 7 15 16 31)) '(1 7 31))
(test (felices '(1 2 3 4 5 6 7 18 19 20)) '(1 7 19))


;; Función que encuentra el factorión de un número.
;; factorionr: number -> number
(define (factorionr n)
  (letrec([factorial(lambda (n)
                      (if(= n 0) 
                         1
                         (* n (factorial (- n 1 )))))]
          [number->list (lambda (num)
                        (map (lambda (c) (- (char->integer c) (char->integer #\0))) (string->list (number->string num))))])
    (foldr + 0 (map factorial (number->list n)))))
        
(test (factorionr 145) 145)
(test (factorionr 168) 41041)
(test (factorionr 456) 864)
(test (factorionr 987) 408240)
(test (factorionr 999) 1088640)

;; Función que encuentra el factorión de un número.
;; factorionl: number -> number
(define (factorionl n)
  (letrec([factorial(lambda (n)
                      (if(= n 0) 
                         1
                         (* n (factorial (- n 1 )))))]
          [number->list (lambda (num)
                        (map (lambda (c) (- (char->integer c) (char->integer #\0))) (string->list (number->string num))))])
    (foldl + 0 (map factorial (number->list n)))))

(test (factorionl 145) 145)
(test (factorionl 168) 41041)
(test (factorionl 456) 864)
(test (factorionl 987) 408240)
(test (factorionl 999) 1088640)

