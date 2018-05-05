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
