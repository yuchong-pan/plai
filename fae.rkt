#lang plai

;; <FAE> ::= <num>
;;         | {+ <FAE> <FAE>}
;;         | {with {<id> <FAE>} <FAE>}
;;         | <id>
;;         | {fun {<id>} <FAE>}
;;         | {<FAE> <FAE>}

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)])

;; lookup : symbol DefrdSub -> FAE
;; returns value of given name in deferred substitutions

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (bound-id value rest-ds)
          (if (symbol=? name bound-id)
              value
              (lookup name rest-ds))]))

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

;; add-numbers : num num -> num
;; produces the sum of two given numbers

(define (add-numbers n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

;; interp : FAE DefrdSub -> FAE
;; evaluates given FAE expression to its value

(define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (add-numbers (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body ds)]
    [app (fun-expr arg-expr)
         (local [(define fun-val (interp fun-expr ds))]
           (if (closureV? fun-val)
               (interp (closureV-body fun-val)
                       (aSub (closureV-param fun-val)
                             (interp arg-expr ds)
                             (closureV-ds fun-val)))
               (error 'interp "application on non-closure")))]))

(test (interp (parse '{with {x 3}
                        {with {f {fun {y} {+ x y}}}
                          {with {x 5}
                            {f 4}}}})
              (mtSub))
      (numV 7))

(test (interp (parse '{with {x 3}
                        {fun {y}
                          {+ x y}}})
              (mtSub))
      (closureV 'y
                (add (id 'x) (id 'y))
                (aSub 'x (numV 3) (mtSub))))
