#lang plai

;; <WAE> ::= <num>
;;         | {+ <WAE> <WAE>}
;;         | {- <WAE> <WAE>}
;;         | {with {<id> <WAE>} <WAE>}
;;         | <id>

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

;; parse : sexp -> WAE
;; converts S-expression to WAE

(define (parse sexp)
  (match sexp
    [(? number? n) (num n)]
    [(list '+ lhs rhs) (add (parse lhs)
                            (parse rhs))]
    [(list '- lhs rhs) (sub (parse lhs)
                            (parse rhs))]
    [(list 'with (list name named-expr) body)
     (with name
           (parse named-expr)
           (parse body))]
    [(? symbol? name) (id name)]))

;; subst : WAE symbol WAE -> WAE
;; substitutes second argument with third argument in first argument

(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              expr
              (with bound-id
                    named-expr
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]))

(test (subst (parse '{+ x x}) 'x 5)
      (add (num 5) (num 5)))
(test (subst (parse '{+ x {with {x 3} 20}}) 'x 5)
      (add (num 5)
           (with 'x (num 3) (num 20))))
(test (subst (parse '{+ x {with {x 3} x}}) 'x 5)
      (add (num 5)
           (with 'x (num 3) (id 'x))))
(test (subst (parse '{+ x {with {y 3} x}}) 'x 5)
      (add (num 5)
           (with 'y (num 3) (num 5))))
