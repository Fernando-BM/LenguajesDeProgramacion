#lang plai

(require "grammars.rkt")

;; Analizador semántico.
;; Interpreta el árbol de sintaxis abstracta generado por el desendulzador.
;; interp: FBAE -> FBAE-Value
(define (interp expr env)
  (match expr
    [(id i) (lookup i env)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(op f args) (opV f (foldr (lambda (v l) (cons (interp v env) l)) '() args))]
    [(fun param body) (closureV param body env)]
    [(app fun-expr arg) (appA (interp fun-expr env) arg env)]))

;;Función auxiliar recursiva que evalúa funciones
;;appA: FBAE -> FBAE-Value
(define (appA fun-val args env)
  (match args
    ['() (interp fun-val env)]
    [else
     (appA (getBody fun-val) (cdr args)
           (aSub (getParam fun-val)
                 (interp (car args) env)
                 (getEnv fun-val env)))]))


;;Función auxiliar que da el cuerpo de un closure o función.
;;getBody: FBAE -> FBAE
(define (getBody fun-val)
  (match fun-val
    [(fun pram body) body]
    [(closureV pram body env) body]))

;;Función auxiliar que da el parametro de un closure o función.
;;getParam: FBAE -> FBAE
(define (getParam fun-val)
  (match fun-val
    [(fun param body) param]
    [(closureV param body env) param]))

;;Función auxiliar que da el enviorement de la función ya
;;sea el del closure o el env1.
;;getEnv: FBAE Env-> Env
(define (getEnv fun-val env1)
  (match fun-val
    [(fun pram body) env1]
    [(closureV pram body env) env]))

;; Busca el valor de un identificador en el ambiente.
;; Si el identificador no se encuentra, se genera el error "Identificador libre".
;; lookup: symbol Env -> FBAE-Value
(define (lookup id env)
  (match env
    [(mtSub) (error 'interp "Identificador libre")]
    [(aSub name value rest-env) (if (symbol=? name id) value (lookup id rest-env))]))


;;Función auxiliar que dada un operación y una lista de argumentos
;;y los evalúa
(define (opV f l)
  (let ([result (apply f (map (λ (v) (match v
                                       [(? numV?) (numV-n v)]
                                       [(? boolV?) (boolV-b v)])) l))])
    (if (op-number? (list + - * / mmodulo min max mexpt sqrt ) f)
        (numV result)
        (boolV result))))

;; Función auxiliar que verifica si un elemento pertenece a una lista.
;; lista-contiene: list any -> boolean
(define (op-number? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t (op-number? xs e))]))