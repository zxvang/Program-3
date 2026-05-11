#lang racket

;;; CS 441 — Program 3: The Execution Engine

(require (file "/Users/zack/Desktop/Program 2 for 3.rkt")) ;hard coded path

;;; ENVIRONMENT  (immutable association list)

(define (env-lookup name env)
  (let ([key (if (symbol? name) (symbol->string name) name)])
    (cond
      [(null? env)
       (error (string-append "Unbound variable: " key))]
      [(equal? (caar env) key) (cdar env)]
      [else (env-lookup key (cdr env))])))

(define (env-update name value env)
  (cons (cons name value)
        (filter (lambda (p) (not (equal? (car p) name))) env)))

;;; INTERPRETER

;; eval-expr : parser-expr Env -> Number
(define (eval-expr node env)
  (match node
    [(? number? n)   n]
    [(? symbol? s)   (env-lookup (symbol->string s) env)]
    [`(+ ,l ,r) (+  (eval-expr l env) (eval-expr r env))]
    [`(- ,l ,r) (-  (eval-expr l env) (eval-expr r env))]
    [`(* ,l ,r) (*  (eval-expr l env) (eval-expr r env))]
    [`(/ ,l ,r)
     (let ([d (eval-expr r env)])
       (when (zero? d) (error "Division by zero"))
       (/ (eval-expr l env) d))]
    [other (error (format "eval-expr: unknown node ~a" other))]))

;; eval-comp : parser-comp Env -> Boolean
(define (eval-comp node env)
  (match node
    [`(,op ,l ,r)
     (let ([lv (eval-expr l env)]
           [rv (eval-expr r env)])
       (unless (and (number? lv) (number? rv))
         (error "Comparison requires numeric operands"))
       (case op
         [(gt)  (>  lv rv)]
         [(lt)  (<  lv rv)]
         [(gte) (>= lv rv)]
         [(lte) (<= lv rv)]
         [(eq)  (=  lv rv)]
         [(neq) (not (= lv rv))]
         [else  (error (format "eval-comp: unknown op ~a" op))]))]))

(define (exec-stmt-list stmts env)
  (if (null? stmts) env
      (exec-stmt-list (cdr stmts) (exec-stmt (car stmts) env))))

;; exec-stmt : parser-stmt Env -> Env
(define (exec-stmt node env)
  (match node
    [`(assign ,name ,expr)
     (env-update name (eval-expr expr env) env)]
    [`(print ,expr)
     (displayln (eval-expr expr env))
     env]
    ;; If-then-else
    [`(if ,comp (then . ,then-stmts) (else . ,else-stmts))
     (if (eval-comp comp env)
         (exec-stmt-list then-stmts env)
         (exec-stmt-list else-stmts env))]
    ;; If-then only
    [`(if ,comp (then . ,then-stmts))
     (if (eval-comp comp env)
         (exec-stmt-list then-stmts env)
         env)]
    ;; While — parser spreads body as (while cmp s1 s2 ...)
    [`(while ,comp . ,body)
     (exec-while comp body env)]
    [other (error (format "exec-stmt: unknown ~a" other))]))

(define (exec-while comp body env)
  (if (eval-comp comp env)
      (exec-while comp body (exec-stmt-list body env))
      env))

(define (interpret ast)
  (match ast
    [`(program . ,stmts) (exec-stmt-list stmts '())]
    [other (error (format "interpret: bad AST ~a" other))]))

;;; TOP-LEVEL ENTRY POINT: run : String -> Env

(define (run source-string)
  (let* ([parser-ast (run-parser source-string)]
         [final-env  (interpret parser-ast)])
    final-env))

;;; DEMO

(define (run-demo src)
  (run src))

(define (run-demo-error src)
  (with-handlers ([exn:fail? (lambda (e) (printf (exn-message e)))])
    (run src)))

(displayln " CS 441 Program 3: Execution Engine ")

(displayln "Assign and Print")
(run-demo 
  "x := 5; y := x * 2; PRINT y;")
(newline)

(displayln "While Countdown")
(run-demo 
  "x := 3;
   WHILE x > 0 DO
     PRINT x;
     x := x - 1;
   END")
(newline)

(displayln "If-Then-Else Scope")
(run-demo 
  "x := 10;
   IF x > 5 THEN
     y := 20;
   ELSE
     y := 30;
   END
   PRINT y;")
(newline)

(displayln "Nested While + If")
(run-demo 
  "i := 1;
   WHILE i <= 5 DO
     IF i = 3 THEN
       PRINT i;
     END
     i := i + 1;
   END")
(newline)

(displayln "Parenthesised Arithmetic")
(run-demo 
  "x := (3 + 4) * 2;")
(newline)

(displayln "Floating Point")
(run-demo 
  "x := 1.5; y := x * 2.0;")
(newline)

(displayln "Unbound Variable")
(run-demo-error 
  "PRINT z;")
(newline)

(displayln "Division by Zero")
(run-demo-error 
  "x := 10 / 0;")
(newline)

(displayln "Comment Stripping")
(run-demo 
  "/* set x */ x := 7; PRINT x;")
(newline)