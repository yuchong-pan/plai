#lang plai

;; <F1WAE> ::= <num>
;;           | {+ <F1WAE> <F1WAE>}
;;           | {with {<id> <F1WAE>} <F1WAE>}
;;           | <id>
;;           | {<id> <F1WAE>}

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [with (name symbol?)
        (named-expr F1WAE?)
        (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)
       (arg-expr F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (ds DefrdSub?)])

;; parse : sexp -> F1WAE
;; converts given S-expression to an F1WAE expression

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
    [(list (? symbol? fun-name) arg-expr)
     (app fun-name
          (parse arg-expr))]))

;; lookup : symbol DefrdSub -> F1WAE
;; looks up given name in deferred substitutions

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))

;; lookup-fundef : symbol (listof fundef) -> fundef
(define (lookup-fundef fun-name fun-defs)
  (cond [(empty? fun-defs)
         (error 'lookup-fundef "undefined function")]
        [(symbol=? fun-name
                   (fundef-fun-name (first fun-defs)))
         (first fun-defs)]
        [else
          (lookup-fundef fun-name
                         (rest fun-defs))]))

;; interp : F1WAE (listof fundef) DefrdSub -> number
;; evaluates given F1WAE expression to its value

(define (interp expr fun-defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs ds)
                  (interp r fun-defs ds))]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  fun-defs
                  (aSub bound-id
                        (interp named-expr
                                fun-defs
                                ds)
                        ds))]
    [id (v) (lookup v ds)]
    [app (fun-name arg-expr)
         (local [(define the-fun-def (lookup-fundef fun-name fun-defs))]
           (interp (fundef-body the-fun-def)
                   fun-defs
                   (aSub (fundef-arg-name the-fun-def)
                         (interp arg-expr
                                 fun-defs
                                 ds)
                         ds)))]))
