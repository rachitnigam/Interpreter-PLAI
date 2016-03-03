#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])

;; unique?: (list -> sym) -> (list -> list)
;; consumes a function to access the elements that are to be compared to give a function
;; checks all the accessed elements in the list are unique throws an error if the list does not have
;; uniques elements, otherwise returns the list itself
(define (unique? accessor name)
  (define (unique-helper accessor element)
    (λ(lst)
      (cond
        [(null? lst) element]
        [(eq? (accessor element) (accessor (first lst)))
         (error 'parse "multiple occurances of ~a in ~a~n" (accessor element) name)]
        [else ((unique-helper accessor element) (rest lst))])))
  (λ(lst)
    (if(null? lst) '()
       (cons ((unique-helper accessor (first lst)) (rest lst)) ((unique? accessor name) (rest lst))))))

;; parse: S-expression -> CFWAE
;; Consumes an S-expression to give the corresponding CFWAE
(define (parse sexp)
  (match sexp
    ['() '()]
    [(? number? n) (num n)]
    [(? symbol? a) (id a)]
    [`(+ ,arg1 ,arg2) (binop + (parse arg1) (parse arg2))]
    [`(- ,arg1 ,arg2) (binop - (parse arg1) (parse arg2))]
    [`(* ,arg1 ,arg2) (binop * (parse arg1) (parse arg2))]
    [`(/ ,arg1 ,arg2) (binop / (parse arg1) (parse arg2))]
    [`(if0 ,c ,t ,e) (if0 (parse c) (parse t) (parse e))]
    [`(with (,bindings ...) ,body) (with (parse ((unique? first 'with-binding) bindings)) (parse body))]
    [`( ( ,(? symbol? sym) ,b) ,c ...) (cons (binding sym (parse b)) (parse c))] 
    [`(fun (,args ...) ,body) (fun ((unique? (λ(lst) lst) 'argument-list) args) (parse body))]
    [`(,func-name ,args ...) (app (parse func-name) (map parse args))]))

;; interp-env : CFWAE -> CFWAE-Value
;; This procedure   interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closuerV or a numV)
(define (interp expr)
  (define (interp-env expr env)
    ;; binop-apply: (number number -> number) numV numV -> numV
    ;; applies a binary operation on the given arguments
    (define (binop-apply op arg1 arg2)
      (if (and (numV? arg1) (numV? arg2))
          (numV (op (numV-n arg1) (numV-n arg2)))
          (error 'binop-apply "binary operations require numV types")))
    
    ;; add-bindings : (listof bindings) Env -> Env
    ;; adds each binding from a list to the Environment
    ;; Assumes each binding identifier in the list is unique
    (define (add-bindings binding-lst envir original-envir)
      (if(null? binding-lst) envir
         (local ([define bind (first binding-lst)])
           (add-bindings (rest binding-lst)
                         (anEnv (binding-name bind) ( interp-env (binding-named-expr bind) original-envir) envir)
                         original-envir))))
    
    ;; lookup: symbol Env -> CFWAE
    ;; returns CFWAE expression for a given identifier
    (define (lookup sym envir)
      (type-case Env envir 
        [mtEnv () (error 'lookup "free identifier (~a) in Environment" sym)]
        [anEnv (name val rest-envir) (if(symbol=? name sym) val
                                        (lookup sym rest-envir))]))
    
    ;; add-func-bindings: (listof symbol) (listof CFWAE) Env -> Env
    ;; Consumes a list of symbols and a list of CFWAE and adds to the Env
    ;; throws an error if the lengths of the two lists are different
    (define (add-func-bindings param-lst arg-lst env original-env)
      (cond
        [(and (null? param-lst) (null? arg-lst)) env]
        [(eq? (length param-lst) (length arg-lst))
         (add-func-bindings (rest param-lst)
                            (rest arg-lst)
                            (anEnv (first param-lst) ( interp-env (first arg-lst) original-env) env)
                            original-env)]
        [else (error ' interp-env "argument list (~a) does not have required number of elements (~a)" (length param-lst) (length arg-lst))]))
    
    (type-case CFWAE expr
      [num (n) (numV n)]
      [binop (op l r) (binop-apply op ( interp-env l env) ( interp-env r env))]
      [with (lst body) ( interp-env body (add-bindings lst env env))]
      [id (v) (lookup v env)]
      [if0 (c t e) (if(= (numV-n ( interp-env c env)) 0) ( interp-env e env) ( interp-env t env))]
      [fun (args-list body) (closureV args-list body env)]
      [app (func-expr args-list)
           (let* ([fun-val ( interp-env func-expr env)]
                  [local-env (closureV-env fun-val)])
             ( interp-env (closureV-body fun-val)
                          (add-func-bindings (closureV-params fun-val) args-list local-env env)))]))
  (interp-env expr (mtEnv)))


;;---------------------Test for  interp-env---------------------
;;Basic Arithmetic Test
(test (interp (parse '3)) (numV 3)) 
(test (interp (parse '{+ 3 4})) (numV 7))
(test (interp (parse '{+ 1 {+ 2 3}})) (numV 6))

;;Basic 'with' usage and scope tests
(test (interp (parse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (interp (parse '{with {{x 5}} {+ x x}})) (numV 10))
(test (interp (parse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (interp (parse '{with {{x 5}} {with {{y {- x 3}}} {+ y y}}})) (numV 4))
(test (interp (parse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (interp (parse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (interp (parse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (interp (parse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (interp (parse '{with {{x 5}} {with {{x x}} x}})) (numV 5))

;;Advanced 'with' usage - Multiple substitutions and Scope Tests
(test (interp (parse '{with {{x 2} {y 1}} {+ x y}}) ) (numV 3))
(test (interp (parse '{with {{x 1} {y 2} {z 3}} {+ x {+ y z}}}) ) (numV 6))
(test (interp (parse '{with {{x 2} {y 3}} {with {{x {* 2 x}} {y {* 2 y}}} {+ x y}}}) ) (numV 10))

;; function and conditional tests
(test (interp (parse '{{fun (x y) (+ x y)} 3 4}) ) (numV 7))
(test (interp (parse '{with {{x 2} {y 3}} {if0 {- x y} {+ x y} {* x y}}}) ) (numV 5))
(test (interp (parse '{with {{x 2} {y 2}} {if0 {- x y} {+ x y} {* x y}}}) ) (numV 4))
(test (interp(parse '{{fun {x y} {if0 {- {* x x} y}
                                      {with {{k {* 2 x}}} {* k k}}
                                      {with {{k {* 2 x}} {l {* 2 y}}} {+ k l}}}} 5 6}) ) (numV 100))
(test (interp(parse '{{fun {x y} {if0 {- {* x x} y}
                                      {with {{k {* 2 x}}} {* k k}}
                                      {with {{k {* 2 x}} {l {* 2 y}}} {+ k l}}}} 5 25}) ) (numV 60))
(test (interp (parse '{with {{x 3}} { with {{f {fun {y} {+ x y}}}} {with {{x 5}} {f 4}}}}) ) (numV 7))
(test (interp (parse '{with {{double {fun {x} {+ x x}}}} {double {double 3}}})) (numV 12))
(test (interp (parse '{with {{x 3}} {with {{f {fun {y} {+ x y}}}} {with {{x 5}} {f x}}}})) (numV 8))
(test (interp (parse '{with {{double {fun (x) {+ x x}}}
                             {square {fun (x) (* x x)}}}
                            {with {{sum-square {fun {x y} {+ {square x} {square y}}}}
                                   {sum-map-square {fun {func x y} {+ {func x} {func y}}}}}
                                  {sum-map-square double 5 12}}})) (numV 34))
(test (interp (parse '{with {{double {fun (x) {+ x x}}}
                             {square {fun (x) (* x x)}}}
                            {with {{sum-square {fun {x y} {+ {square x} {square y}}}}
                                   {sum-map-square {fun {func x y} {+ {func x} {func y}}}}}
                                  {sum-map-square square 5 12}}})) (numV 169))
