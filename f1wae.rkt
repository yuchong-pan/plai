#lang plai

;; <F1WAE> ::= <num>
;;           | {+ <F1WAE> <F1WAE>}
;;           | {with {<id> <F1WAE>} <F1WAE>}
;;           | <id>
;;           | {<id> <F1WAE>}

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

;; parse : sexp -> F1WAE
;; converts S-expression to a F1WAE expression

(define (parse sexp)
  (match sexp
    [(? number? n) (num n)]
    [(list '+ lhs rhs) (add (parse lhs)
                            (parse rhs))]
    [(list 'with (list (? symbol? name) named-expr) body)
     (with name
           (parse named-expr)
           (parse body))]
    [(? symbol? name) (id name)]
    [(list (? symbol? fun-name) arg)
     (app fun-name (parse arg))]))

;; subst : F1WAE symbol F1WAE -> F1WAE
;; substitutes sub-id in expr by val

(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (with bound-id
                (subst named-expr sub-id val)
                (if (symbol=? bound-id sub-id)
                    bound-body
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val (id v))]
    [app (fun-name arg-expr)
         (app fun-name
              (subst arg-expr sub-id val))]))

;; lookup-fundef : symbol (listof fundef) -> fundef | bool
;; returns function of given name in fun-defs, or false if not found

(define (lookup-fundef fun-name fun-defs)
  (cond [(empty? fun-defs)
         (error 'lookup-fundef "undefined function")]
        [(symbol=? fun-name (fundef-fun-name (first fun-defs)))
         (first fun-defs)]
        [else
          (lookup-fundef fun-name (rest fun-defs))]))

;; interp : F1WAE (listof fundef) -> number
;; evaluates F1WAE expression to number

(define (interp expr fun-defs)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs)
                  (interp r fun-defs))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (num (interp named-expr fun-defs)))
                  fun-defs)]
    [id (v) (error 'interp "free identifier")]
    [app (fun-name arg-expr)
         (local [(define the-fun-def (lookup-fundef fun-name fun-defs))]
           (interp (subst (fundef-body the-fun-def)
                          (fundef-arg-name the-fun-def)
                          (num (interp arg-expr fun-defs)))
                   fun-defs))]))

(test (interp (parse '{double {double 5}})
              (list (fundef 'double
                            'n
                            (add (id 'n) (id 'n)))))
      20)
