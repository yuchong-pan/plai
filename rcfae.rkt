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
