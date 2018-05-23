#lang plai

;; <CFAE/L> ::= <num>
;;            | {+ <CFAE/L> <CFAE/L>}
;;            | <id>
;;            | {fun {<id>} <CFAE/L>}
;;            | {<CFAE/L> <CFAE/L>}

(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun CFAE/L?) (arg CFAE/L?)])

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?) (body CFAE/L?) (env Env?)]
  [exprV (body CFAE/L?) (env Env?) (cache boxed-boolean/CFAE/L-Value?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value CFAE/L-Value?) (next Env?)])

;; boxed-boolean/CFAE/L-Value? : any -> boolean
;; returns true if given value is a boxed boolean or boxed CFAE/L-Value

(define (boxed-boolean/CFAE/L-Value? v)
  (and (box? v)
       (or (boolean? (unbox v))
           (numV? (unbox v))
           (closureV? (unbox v)))))

;; strict : CFAE/L-Value -> CFAE/L-Value
;; strictly evaluates given CFAE/L-Value expression

(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (body env cache)
           (if (boolean? (unbox cache))
               (local [(define the-value (strict (interp body env)))]
                 (begin
                   (printf "Forcing exprV ~a to ~a~n" e the-value)
                   (set-box! cache the-value)
                   the-value))
               (begin
                 (printf "Using cached value~n")
                 (unbox cache)))]
    [else e]))

;; num+ : CFAE/L-Value CFAE/L-Value -> numV
;; adds two given values and produces sum

(define (num+ n1 n2)
  (numV (+ (numV-n (strict n1)) (numV-n (strict n2)))))

;; lookup : symbol Env -> CFAE/L-Value
;; looks up given name in env

(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "free identifier")]
    [aSub (sub-id value next)
          (if (symbol=? name sub-id)
              value
              (lookup name next))]))

;; parse : sexp -> CFAE/L
;; converts given S-expression to CFAE/L expression

(define (parse sexp)
  (match sexp
    [(? number? n) (num n)]
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [(? symbol? v) (id v)]
    [(list 'fun (list (? symbol? param)) body)
     (fun param (parse body))]
    [(list fun arg) (app (parse fun) (parse arg))]
    [(list 'with (list (? symbol? name) named-expr) body)
     (app (fun name (parse body)) (parse named-expr))]))

;; interp : CFAE/L Env -> CFAE/L-Value
;; evaluates given CFAE/L expression to its value

(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (param body) (closureV param body env)]
    [app (fun arg)
         (local [(define fun-val (strict (interp fun env)))]
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (exprV arg env (box false))
                         (closureV-env fun-val))))]))
