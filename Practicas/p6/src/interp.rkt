#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")

;; Analizador semántico.
;; Interpreta el árbol de sintaxis abstracta generado por el desendulzador.
;; interp: RCFBAEL/L -> RCFBAEL/L-Value
(define (interp expr env)
  (match expr
    [(id i) (lookup i env)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(lisT elems) (listV (map (lambda (v) (strict (interp v env))) elems))]
    [(op f args) (opV f (map (lambda (v) (strict (interp v env))) args))]
    [(iF expr then-expr else-expr) (if (boolV-b (strict (interp expr env)))
                                       (interp then-expr env)
                                       (interp else-expr env))]
    [(fun param body) (closureV param body env (make-hash empty))]
    [(rec bindings  body) (interp body (cyclically-bind-and-interp  bindings env))]
    [(app fun-expr arg)
     (let([fun-val (strict (interp fun-expr env))])
       (let([resultado (hash-ref (closureV-tbl fun-val) (closureV-param fun-val) 'ninguno)])
         (cond
           [(equal? resultado 'ninguno)
            (define nuevo (appA (strict (interp fun-expr env)) arg env))
            (hash-set! (closureV-tbl fun-val) (closureV-param fun-val) nuevo)
            nuevo]
           [else resultado])))]))

;;Función auxiliar recursiva que evalúa funciones
;;appA: FBAE -> FBAE-Value
(define (appA fun-val args env)
  (match args
    ['() (strict(interp fun-val env))]
    [else
     (appA (getBody fun-val) (cdr args)
           (aSub (getParam fun-val)
                 (exprV (car args) env)
                 (getEnv fun-val env)))]))


;;Función auxiliar que da el cuerpo de un closure o función.
;;getBody: FBAE -> FBAE
(define (getBody fun-val)
  (match fun-val
    [(fun pram body) body]
    [(closureV pram body env hash) body]))

;;Función auxiliar que da el parametro de un closure o función.
;;getParam: FBAE -> FBAE
(define (getParam fun-val)
  (match fun-val
    [(fun param body) param]
    [(closureV param body env hash) param]))

;;Función auxiliar que da el enviroment de la función ya
;;sea el del closure o el env1.
;;getEnv: FBAE Env-> Env
(define (getEnv fun-val env1)
  (match fun-val
    [(fun pram body) env1]
    [(closureV pram body env hash) env]))

;; Busca el valor de un identificador en el ambiente.
;; Si el identificador no se encuentra, se genera el error "Identificador libre".
;; lookup: symbol Env -> RCFBAEL/L-Value
(define (lookup id env)
  (match env
    [(mtSub) (error 'interp (string-append "Identificador libre " (symbol->string id)))]
    [(aSub name value rest-env) (if (symbol=? name id) value (lookup id rest-env))]
    [(aRecSub name value rest-env) (if (symbol=? id name) (unbox value) (lookup id rest-env))]))


;;Función auxiliar que dada un operación y 
;;una lista de argumentos los evalúa
(define (opV f l)
  (let ([result (apply f (map (lambda (v) (match v
                                            [(? numV?) (numV-n v)]
                                            [(? boolV?) (boolV-b v)]
                                            [(? listV?) (listV-elems v)])) l))])
    (cond
      [(op-number? (list + - * /  mmodulo min max mexpt sqrt sub1 add1) f) (numV result)]
      [(op-number? (list  cdr append) f) (listV result)]
      [(op-number? (list car) f) result]
      [else (boolV result)])))

;; Función auxiliar que verifica si un elemento pertenece a una lista.
;; lista-contiene: list any -> boolean
(define (op-number? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t (op-number? xs e))]))

;; Fuerza la evaluación de un punto estricto.
;; strict: RCFBAEL/L-Value -> RCFBAEL/L-Value
(define (strict expr)
  (match expr
    [(exprV expr env) (strict (interp expr env))]
    [else expr]))

;; Función que crea el ambiente recursivo.
;; cyclically-bind-and-interp: listof Binding -> Enviroment
(define (cyclically-bind-and-interp bindings env)
  (if (empty? bindings)
      env
      ; Creamos la caja que almacenará la función recursiva
      (let* ([value-holder (box (numV 1729))]
             ; Creamos el ambiente y lo asociamos a la caja anterior.
             [new-env (aRecSub (binding-name (car bindings)) value-holder env)]
             ; Interpretamos el valor, esto nos devuelve un clousure que tendrá como ambiente el new-env
             [named-expr-val (interp (binding-value (car bindings)) new-env)])
        (begin
          ; Modificamos la caja. Ahora en lugar de almacenar 1729 almacena el closure con la función
          ; recursiva.
          (set-box! value-holder named-expr-val)
          ; Regresamos el ambiente. La caja del ambiente tiene la nueva modificación.
          (cyclically-bind-and-interp (cdr bindings) new-env)))))

