#lang plai

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)])

;; number-or-procedure? : any -> bool
;; returns true if the given value is a number or a procedure

(define (number-or-procedure? v)
  (or (number? v)
      (procedure? v)))

;; Env? : any -> bool
;; returns true if the given value is an environment

(define (Env? x)
  (procedure? x))

;; aSub : symbol number-or-procedure Env -> Env
;; returns an environment that extends the given environment with the given name and value

(define (aSub bound-name bound-value env)
  (lambda (want-name)
    (cond
      [(symbol=? want-name bound-name) bound-value]
      [else (lookup want-name env)])))

;; lookup : symbol Env -> number-or-procedure
;; looks up the given name in env

(define (lookup name env)
  (env name))

;; mtSub : -> Env
;; returns the empty environment that raises an error for any name

(define (mtSub)
  (lambda (name)
    (error 'lookup "no binding for identifier")))

;; parse : sexp -> FAE
;; converts given S-expression to its FAE expression

(define (parse sexp)
  (match sexp
    [(? number? n) (num n)]
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [(list 'with (list (? symbol? name) named-expr) body)
     (app (fun name (parse body))
          (parse named-expr))]
    [(? symbol? name) (id name)]
    [(list 'fun (list (? symbol? param)) body)
     (fun param (parse body))]
    [(list fun-expr arg-expr)
     (app (parse fun-expr) (parse arg-expr))]))

;; interp : FAE Env -> number-or-procedure
;; evaluates given FAE expression to its value

(define (interp expr env)
  (type-case FAE expr
    [num (n) n]
    [add (l r) (+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (lambda (arg-val)
           (interp bound-body
                   (aSub bound-id arg-val env)))]
    [app (fun-expr arg-expr)
         (local [(define fun-val (interp fun-expr env))
                 (define arg-val (interp arg-expr env))]
           (fun-val arg-val))]))

(test (interp (parse '{with {x 3}
                        {with {f {fun {y} {+ x y}}}
                          {with {x 5}
                            {f 4}}}})
              (mtSub))
      7)
