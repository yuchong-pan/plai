#lang plai

;; <RCFAE> ::= <num>
;;           | {+ <RCFAE> <RCFAE>}
;;           | {* <RCFAE> <RCFAE>}
;;           | <id>
;;           | {fun {<id>} <RCFAE>}
;;           | {<RCFAE> <RCFAE>}
;;           | {if0 <RCFAE> <RCFAE> <RCFAE>}
;;           | {rec {<id> <RCFAE>} <RCFAE>}

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [mul (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
  [if0 (pred RCFAE?) (truth RCFAE?) (falsity RCFAE?)]
  [rec (name symbol?) (named-expr RCFAE?) (body RCFAE?)])

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RCFAE?) (env Env?)])

;; boxed-RCFAE-Value? : any -> boolean
;; returns true if given value is a boxed RCFAE-Value

(define (boxed-RCFAE-Value? value)
  (and (box? value)
       (RCFAE-Value? (unbox value))))

(define-type Env
  [mtSub]
  [aSub (key symbol?) (value RCFAE-Value?) (next Env?)]
  [aRecSub (key symbol?) (value boxed-RCFAE-Value?) (next Env?)])

;; parse : sexp -> RCFAE
;; converts given S-expression to RCFAE expression

(define (parse sexp)
  (match sexp
    [(? number? n) (num n)]
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [(list '* lhs rhs) (mul (parse lhs) (parse rhs))]
    [(? symbol? v) (id v)]
    [(list 'fun (list (? symbol? param)) body)
     (fun param (parse body))]
    [(list fun-expr arg-expr) (app (parse fun-expr) (parse arg-expr))]
    [(list 'if0 pred truth falsity)
     (if0 (parse pred) (parse truth) (parse falsity))]
    [(list 'with (list (? symbol? name) named-expr) body)
     (app (fun name (parse body)) (parse named-expr))]
    [(list 'rec (list (? symbol? name) named-expr) body)
     (local [(define parsed-named-expr (parse named-expr))]
       (if (fun? parsed-named-expr)
           (rec name (parse named-expr) (parse body))
           (error 'parse "named expression of rec not a procedure")))]))

;; num-op : op numV numV -> numV
;; applies given operation on given two numbers

(define (num-op op n1 n2)
  (numV (op (numV-n n1) (numV-n n2))))

;; num-zero? : numV -> boolean
;; returns true if given numV value is zero

(define (num-zero? n)
  (zero? (numV-n n)))

;; lookup : symbol Env -> RCFAE-Value
;; looks up given name in given environment

(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "free identifier")]
    [aSub (key value next)
          (if (symbol=? name key)
              value
              (lookup name next))]
    [aRecSub (key boxed-value next)
             (if (symbol=? name key)
                 (unbox boxed-value)
                 (lookup name next))]))

;; cyclically-bind-and-interp : symbol fun env -> env
;; returns an environment that associates bound-id with a closure whose environment is the containing environment

(define (cyclically-bind-and-interp bound-id named-expr env)
  (local [(define value-holder (box (numV 1729)))
          (define new-env (aRecSub bound-id value-holder env))
          (define named-expr-val (interp named-expr new-env))]
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

;; interp : RCFAE Env -> RCFAE-Value
;; evaluates given RCFAE expression to its value

(define (interp expr env)
  (type-case RCFAE expr
    [num (n) (numV n)]
    [add (l r) (num-op + (interp l env) (interp r env))]
    [mul (l r) (num-op * (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (param body) (closureV param body env)]
    [app (fun-expr arg-expr)
         (local [(define fun-val (interp fun-expr env))
                 (define arg-val (interp arg-expr env))]
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         arg-val
                         (closureV-env fun-val))))]
    [if0 (pred truth falsity)
         (local [(define pred-val (interp pred env))]
           (if (num-zero? pred-val)
               (interp truth env)
               (interp falsity env)))]
    [rec (bound-id named-expr bound-body)
         (interp bound-body
                 (cyclically-bind-and-interp bound-id
                                             named-expr
                                             env))]))

(test (interp (parse '{rec {fac {fun {x}
                                     {if0 x
                                          1
                                          {* x {fac {+ x -1}}}}}}
                           {fac 10}})
              (mtSub))
      (numV 3628800))
