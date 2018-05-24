#lang plai

;; <CFAE/L> ::= <num>
;;            | {+ <CFAE/L> <CFAE/L>}
;;            | <id>
;;            | {fun {<id>} <CFAE/L>}
;;            | {<CFAE/L> <CFAE/L>}
;;            | {if0 <CFAE/L> <CFAE/L> <CFAE/L>}
;;            | nil
;;            | {pair <CFAE/L> <CFAE/L>}
;;            | {head <CFAE/L>}
;;            | {tail <CFAE/L>}
;;            | {mt? <CFAE/L>}
;;            | {rec <id> {<id>} <CFAE/L>}

(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun CFAE/L?) (arg CFAE/L?)]
  [if0 (pred CFAE/L?) (truth CFAE/L?) (falsity CFAE/L?)]
  [nil]
  [pair (head CFAE/L?) (tail CFAE/L?)]
  [head (lst CFAE/L?)]
  [tail (lst CFAE/L?)]
  [mt? (lst CFAE/L?)]
  [rec (name symbol?) (param symbol?) (body CFAE/L?)])

(define-type CFAE/L-Value
  [numV (n number?)]
  [nilV]
  [pairV (head CFAE/L-Value?) (tail CFAE/L-Value?)]
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
    [(list 'fun (list (? symbol? param)) body)
     (fun param (parse body))]
    [(list 'if0 pred truth falsity)
     (if0 (parse pred) (parse truth) (parse falsity))]
    ['nil (nil)]
    [(list 'pair head tail) (pair (parse head) (parse tail))]
    [(list 'head lst) (head (parse lst))]
    [(list 'tail lst) (tail (parse lst))]
    [(list 'mt? lst) (mt? (parse lst))]
    [(? symbol? v) (id v)]
    [(list fun arg) (app (parse fun) (parse arg))]
    [(list 'rec (? symbol? name) (list (? symbol? param)) body)
     (rec name param (parse body))]))

;; num-zero? : CFAE/L-Value -> boolean
;; returns true if given number is 0

(define (num-zero? n)
  (= 0 (numV-n (strict n))))

;; num+ : CFAE/L-Value CFAE/L-Value -> numV
;; adds two given numbers

(define (num+ n1 n2)
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
           (local [(define the-value (strict (interp expr env)))]
             (begin
               (printf "Forcing exprV to ~a~n" the-value)
               the-value))]
    [else val]))

;; interp : CFAE/L Env -> CFAE/L-Value
;; evaluates given CFAE/L expression to its value

(define (interp expr env)
  (local [(define (interp-helper expr env)
            (type-case CFAE/L expr
              [num (n) (numV n)]
              [add (l r) (num+ (interp-helper l env) (interp-helper r env))]
              [id (v) (lookup v env)]
              [fun (param body) (closureV param body env)]
              [app (fun arg)
                   (local [(define fun-val (strict (interp-helper fun env)))
                           (define arg-val (exprV arg env))]
                     (interp-helper (closureV-body fun-val)
                                    (aSub (closureV-param fun-val)
                                          arg-val
                                          (closureV-env fun-val))))]
              [if0 (pred truth falsity)
                   (local [(define pred-val (interp-helper pred env))]
                     (if (num-zero? pred-val)
                         (interp truth env)
                         (interp falsity env)))]
              [nil () (nilV)]
              [pair (head tail)
                    (pairV (interp head env) (interp tail env))]
              [head (lst)
                    (local [(define lst-val (strict (interp lst env)))]
                      (pairV-head lst-val))]
              [tail (lst)
                    (local [(define lst-val (strict (interp lst env)))]
                      (pairV-tail lst-val))]
              [mt? (lst)
                   (local [(define lst-val (strict (interp lst env)))]
                     (if (nilV? lst-val)
                         (numV 1)
                         (numV 0)))]
              [rec (name param body)
                   (local [(define closure-val (closureV param body env))]
                     (begin
                       (set-closureV-env! closure-val
                                          (aSub name
                                                closure-val
                                                env))
                       closure-val))]))]
    (strict (interp-helper expr env))))

(test (interp (parse '{with {sum {rec sum {x}
                                   {if0 x
                                        0
                                        {+ x
                                           {sum {+ x -1}}}}}}
                            {sum 10}})
              (mtSub))
      (numV 55))
(test (interp (parse '{with {fib {rec fib {x}
                                   {if0 x
                                        0
                                        {if0 {+ x -1}
                                             1
                                             {+ {fib {+ x -1}}
                                                {fib {+ x -2}}}}}}}
                            {fib 10}})
              (mtSub))
      (numV 55))
(test (interp (parse '{with {sum {rec sum {x}
                                   {if0 {mt? x}
                                        {+ {head x}
                                           {sum {tail x}}}
                                        0}}}
                            {sum {pair 5 {pair 4 {pair 3 {pair 2 {pair 1 nil}}}}}}})
              (mtSub))
      (numV 15))
