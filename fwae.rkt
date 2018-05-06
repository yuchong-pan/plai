#lang plai

;; <FWAE> ::= <num>
;;          | {+ <FWAE> <FWAE>}
;;          | {with {<id> <FWAE>} <FWAE>}
;;          | <id>
;;          | {fun {<id>} <FWAE>}
;;          | {<FWAE> <FWAE>}

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)]
  [app (fun-expr FWAE?) (arg-expr FWAE?)])

;; parse : sexp -> FWAE
;; parses given S-expression to its FWAE expression

(define (parse sexp)
  (match sexp
    [(? number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'with (list (? symbol? name) named-expr) body)
     (with name (parse named-expr) (parse body))]
    [(? symbol? name) (id name)]
    [(list 'fun (list (? symbol? param)) body)
     (fun param (parse body))]
    [(list fun-expr arg-expr)
     (app (parse fun-expr) (parse arg-expr))]
    [_ (error 'parse "syntax error")]))

;; add-numbers : num num -> num
;; adds two nums and produces a new num representing the sum

(define (add-numbers n1 n2)
  (num (+ (num-n n1) (num-n n2))))

;; subst : FWAE symbol FWAE -> FWAE
;; substitutes given name in expr with given value

(define (subst expr sub-id val)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (with bound-id
                (subst named-expr sub-id val)
                (if (symbol=? bound-id sub-id)
                    bound-body
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [fun (bound-id bound-body)
         (if (symbol=? bound-id sub-id)
             expr
             (fun bound-id
                  (subst bound-body sub-id val)))]
    [app (fun-expr arg-expr)
         (app (subst fun-expr sub-id val)
              (subst arg-expr sub-id val))]))

;; interp : FWAE -> FWAE
;; evaluates given FWAE expression to its value

(define (interp expr)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add-numbers (interp l) (interp r))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (interp named-expr)))]
    [id (v) (error 'interp "free identifier")]
    [fun (bound-id bound-body) expr]
    [app (fun-expr arg-expr)
         (local [(define fun-val (interp fun-expr))]
           (interp (subst (fun-body fun-val)
                          (fun-param fun-val)
                          (interp arg-expr))))]))

(test (interp (parse '{with {x 3}
                        {fun {y}
                          {+ x y}}}))
      (fun 'y (add (num 3) (id 'y))))
