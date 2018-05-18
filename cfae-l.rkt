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
  [exprV (expr CFAE/L?) (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (named-expr CFAE/L-Value?) (next Env?)])

;; parse : sexp -> CFAE/L
;; converts given S-expression to CFAE/L expression

(define (parse sexp)
  (match sexp
    [(? number? n) (num n)]
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [(list 'with (list (? symbol? name) named-expr) body)
     (app (fun name (parse body)) (parse named-expr))]
    [(? symbol? v) (id v)]
    [(list 'fun (list (? symbol? param)) body)
     (fun param (parse body))]
    [(list fun arg) (app (parse fun) (parse arg))]))

;; add-numbers : CFAE/L-Value CFAE/L-Value -> numV
;; adds two given numbers

(define (add-numbers n1 n2)
  (numV (+ (numV-n (strict n1))
           (numV-n (strict n2)))))

;; lookup : symbol Env -> CFAE/L-Value
;; looks up given name in env

(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "free identifier")]
    [aSub (bound-id val next)
          (if (symbol=? name bound-id)
              val
              (lookup name next))]))

;; strict : CFAE/L-Value -> CFAE/L-Value
;; strictly evaluates given value if it is an exprV

(define (strict val)
  (type-case CFAE/L-Value val
    [exprV (expr env)
           (strict (interp expr env))]
    [else val]))

;; interp : CFAE/L Env -> CFAE/L-Value
;; evaluates given CFAE/L expression to its value

(define (interp expr env)
  (local [(define (interp-helper expr env)
            (type-case CFAE/L expr
              [num (n) (numV n)]
              [add (l r) (add-numbers (interp-helper l env) (interp-helper r env))]
              [id (v) (lookup v env)]
              [fun (param body) (closureV param body env)]
              [app (fun arg)
                   (local [(define fun-val (strict (interp-helper fun env)))
                           (define arg-val (exprV arg env))]
                     (interp-helper (closureV-body fun-val)
                                    (aSub (closureV-param fun-val)
                                          arg-val
                                          (closureV-env fun-val))))]))]
    (strict (interp-helper expr env))))

(test (interp (parse '{with {x {+ 4 5}}
                        {with {y {+ x x}}
                          {with {z y}
                            {with {x 4}
                              z}}}})
              (mtSub))
      (numV 18))
(test (interp (parse '{with {x 3}
                        x})
              (mtSub))
      (numV 3))
(test (interp (parse '{with {x 3}
                        {+ x x}})
              (mtSub))
      (numV 6))
(test (interp (parse '{with {double {fun {x} {+ x x}}}
                        {+ {double 5}
                          {double 10}}})
              (mtSub))
      (numV 30))
(test (interp (parse '{with {f {undef x}}
                        4})
              (mtSub))
      (numV 4))
