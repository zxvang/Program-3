#lang racket

;;; CS 441 — Program 2: Recursive Descent Parser

;;;   source string
;;;     -> strip-comments  (removes /* ... */ blocks)
;;;     -> tokenize        (produces a list of token structs)
;;;     -> parse-program   (recursive descent -> S-expression AST)

(provide strip-comments tokenize token token-type token-value
         run-parser run-file)

;;;=============================================================== SECTION 1 — TOKEN STRUCT

(struct token (type value) #:transparent)

;;;=============================================================== SECTION 2 — STRIP COMMENTS

;; strip-comments : String -> String
(define (strip-comments src)
  (let loop ([chars (string->list src)]
             [in-comment #f]
             [result '()])
    (cond
      ;; End of input
      [(null? chars)
       (when in-comment
         (error 'lexical-error "Unterminated comment — missing '*/'"))
       (list->string (reverse result))]

      ;; Inside a comment: scan for closing */
      [in-comment
       (if (and (char=? (car chars) #\*)
                (pair? (cdr chars))
                (char=? (cadr chars) #\/))
           (loop (cddr chars) #f (cons #\space result))
           (loop (cdr chars) #t result))]

      ;; Outside comment
      [else
       (cond
         ;; Opening /*
         [(and (char=? (car chars) #\/)
               (pair? (cdr chars))
               (char=? (cadr chars) #\*))
          (loop (cddr chars) #t result)]
         ;; Stray */ - Lexical error
         [(and (char=? (car chars) #\*)
               (pair? (cdr chars))
               (char=? (cadr chars) #\/))
          (error 'lexical-error "Unexpected '*/' — not inside a comment")]
         ;; Normal character
         [else
          (loop (cdr chars) #f (cons (car chars) result))])])))


;;;=============================================================== SECTION 3 — SCANNER (TOKENIZER)

;; tokenize : String -> (Listof Token)
(define (tokenize src)
  (let loop ([chars (string->list src)]
             [tokens '()]
             [prev-was-value #f])
    (cond
      [(null? chars)
       (reverse tokens)]

      ;; Whitespace: skip
      [(char-whitespace? (car chars))
       (loop (cdr chars) tokens prev-was-value)]

      ;; := (must check before bare :)
      [(and (char=? (car chars) #\:)
            (pair? (cdr chars))
            (char=? (cadr chars) #\=))
       (loop (cddr chars) (cons (token 'ASSIGN-TOK ":=") tokens) #f)]

      ;; Bare colon
      [(char=? (car chars) #\:)
       (error 'lexical-error "Unexpected ':' — did you mean ':='?")]

      ;; !=
      [(and (char=? (car chars) #\!)
            (pair? (cdr chars))
            (char=? (cadr chars) #\=))
       (loop (cddr chars) (cons (token 'RELOP-TOK "!=") tokens) #f)]

      ;; Bare !
      [(char=? (car chars) #\!)
       (error 'lexical-error "Unexpected '!' — only valid as part of '!='")]

      ;; >=
      [(and (char=? (car chars) #\>)
            (pair? (cdr chars))
            (char=? (cadr chars) #\=))
       (loop (cddr chars) (cons (token 'RELOP-TOK ">=") tokens) #f)]

      ;; <=
      [(and (char=? (car chars) #\<)
            (pair? (cdr chars))
            (char=? (cadr chars) #\=))
       (loop (cddr chars) (cons (token 'RELOP-TOK "<=") tokens) #f)]

      ;; // is not a valid comment
      [(and (char=? (car chars) #\/)
            (pair? (cdr chars))
            (char=? (cadr chars) #\/))
       (error 'lexical-error "'//' is not a valid comment — use /* ... */")]

      ;; Single-char relops
      [(char=? (car chars) #\=)
       (loop (cdr chars) (cons (token 'RELOP-TOK "=")  tokens) #f)]
      [(char=? (car chars) #\>)
       (loop (cdr chars) (cons (token 'RELOP-TOK ">")  tokens) #f)]
      [(char=? (car chars) #\<)
       (loop (cdr chars) (cons (token 'RELOP-TOK "<")  tokens) #f)]

      ;; + : unary (absorbed into number) vs binary ADDOP
      [(char=? (car chars) #\+)
       (if (and (not prev-was-value)
                (pair? (cdr chars))
                (char-numeric? (cadr chars)))
           (let-values ([(num-tok rest) (scan-number (cdr chars) #\+)])
             (loop rest (cons num-tok tokens) #t))
           (loop (cdr chars) (cons (token 'ADDOP-TOK "+") tokens) #f))]

      ;; - : unary vs binary
      [(char=? (car chars) #\-)
       (if (and (not prev-was-value)
                (pair? (cdr chars))
                (char-numeric? (cadr chars)))
           (let-values ([(num-tok rest) (scan-number (cdr chars) #\-)])
             (loop rest (cons num-tok tokens) #t))
           (loop (cdr chars) (cons (token 'ADDOP-TOK "-") tokens) #f))]

      ;; * /
      [(char=? (car chars) #\*)
       (loop (cdr chars) (cons (token 'MULOP-TOK "*") tokens) #f)]
      [(char=? (car chars) #\/)
       (loop (cdr chars) (cons (token 'MULOP-TOK "/") tokens) #f)]

      ;; Parentheses
      [(char=? (car chars) #\()
       (loop (cdr chars) (cons (token 'LPAREN-TOK "(") tokens) #f)]
      [(char=? (car chars) #\))
       (loop (cdr chars) (cons (token 'RPAREN-TOK ")") tokens) #t)]

      ;; Semicolon
      [(char=? (car chars) #\;);;cosmetics
       (loop (cdr chars) (cons (token 'SEMI-TOK ";") tokens) #f)]

      ;; Number literal
      [(char-numeric? (car chars))
       (let-values ([(num-tok rest) (scan-number chars #f)])
         (loop rest (cons num-tok tokens) #t))]

      ;; Identifier or keyword
      [(or (char-alphabetic? (car chars)) (char=? (car chars) #\_))
       (let-values ([(id-str rest) (scan-identifier chars)])
         (let ([tok (classify-word id-str)])
           (loop rest (cons tok tokens)
                 (eq? (token-type tok) 'ID-TOK))))]

      ;; Unknown character
      [else
       (error 'lexical-error
              (format "Unrecognized character: '~a'" (car chars)))])))

;;cosmetics
;; scan-number : (Listof Char) (U Char #f) -> (values Token (Listof Char))
;; Reads an INT or FP literal.  sign-char is #\+, #\-, or #f.
(define (scan-number chars sign-char)
  (define sign-str (if sign-char (string sign-char) ""))
  (let collect-int ([cs chars] [acc '()])
    (cond
      [(and (pair? cs) (char-numeric? (car cs)))
       (collect-int (cdr cs) (cons (car cs) acc))]

      ;; Decimal point -> FP
      [(and (pair? cs) (char=? (car cs) #\.))
       (let ([int-digits (reverse acc)]
             [after-dot  (cdr cs)])
         (when (or (null? after-dot) (not (char-numeric? (car after-dot))))
           (error 'lexical-error
                  (format "Invalid FP '~a~a.' — digit(s) required after '.'"
                          sign-str (list->string int-digits))))
         (let collect-frac ([fcs after-dot] [facc '()])
           (cond
             [(and (pair? fcs) (char-numeric? (car fcs)))
              (collect-frac (cdr fcs) (cons (car fcs) facc))]
             [(and (pair? fcs) (char=? (car fcs) #\.))
              (error 'lexical-error
                     (format "Invalid FP '~a~a.' — multiple decimal points"
                             sign-str (list->string int-digits)))]
             [else
              (let* ([full-str (string-append sign-str
                                              (list->string int-digits)
                                              "."
                                              (list->string (reverse facc)))])
                (values (token 'FP-TOK (string->number full-str)) fcs))])))]

      ;; No decimal -> INT
      [else
       (let* ([int-digits (reverse acc)]
              [full-str   (string-append sign-str (list->string int-digits))])
         (when (and (> (length int-digits) 1) (char=? (car int-digits) #\0))
           (error 'lexical-error
                  (format "Invalid INT '~a' — leading zeros are not allowed" full-str)))
         (values (token 'INT-TOK (string->number full-str)) cs))])))


;; scan-identifier : (Listof Char) -> (values String (Listof Char))
;; Reads an identifier.  A '-' is included only when followed by
;; another valid tail character, so binary minus is never swallowed.
(define (scan-identifier chars)
  (let loop ([cs chars] [acc '()])
    (cond
      [(null? cs)
       (values (list->string (reverse acc)) cs)]
      [(or (char-alphabetic? (car cs))
           (char-numeric?    (car cs))
           (char=? (car cs) #\_))
       (loop (cdr cs) (cons (car cs) acc))]
      [(and (char=? (car cs) #\-)
            (pair? (cdr cs))
            (or (char-alphabetic? (cadr cs))
                (char-numeric?    (cadr cs))
                (char=? (cadr cs) #\_)))
       (loop (cdr cs) (cons (car cs) acc))]
      [else
       (values (list->string (reverse acc)) cs)])))


;; classify-word : String -> Token
(define (classify-word word)
  (match word
    ["IF"    (token 'IF-TOK    word)]
    ["THEN"  (token 'THEN-TOK  word)]
    ["ELSE"  (token 'ELSE-TOK  word)]
    ["END"   (token 'END-TOK   word)]
    ["WHILE" (token 'WHILE-TOK word)]
    ["DO"    (token 'DO-TOK    word)]
    ["PRINT" (token 'PRINT-TOK word)]
    [_       (token 'ID-TOK    word)]))

;;;=============================================================== SECTION 4 — PARSER HELPERS

(define (peek tokens)
  (if (null? tokens) 'EOF (car tokens)))

(define (peek-type tokens)
  (let ([t (peek tokens)])
    (if (eq? t 'EOF) 'EOF (token-type t))))

(define (peek-value tokens)
  (let ([t (peek tokens)])
    (if (eq? t 'EOF) #f (token-value t))))

;; consume : (Listof Token) Symbol -> (Listof Token)
(define (consume tokens expected-type)
  (cond
    [(null? tokens)
     (error 'syntax-error
            (format "Unexpected EOF — expected ~a" expected-type))]
    [(eq? (token-type (car tokens)) expected-type)
     (cdr tokens)]
    [else
     (error 'syntax-error
            (format "Expected ~a but found ~a ('~a')"
                    expected-type
                    (token-type (car tokens))
                    (token-value (car tokens))))]))

;; consume-end : (Listof Token) -> (Listof Token)e
;; Consumes END, then an optional trailing semicolon.
(define (consume-end tokens)
  (let ([rest (consume tokens 'END-TOK)])
    (if (and (pair? rest) (eq? (token-type (car rest)) 'SEMI-TOK))
        (cdr rest)
        rest)))

;;;=============================================================== SECTION 5 — RECURSIVE DESCENT PARSER

;;; Every parse-X function:
;;;   Input  : (Listof Token)
;;;   Output : (values AST-Node (Listof Token))
;;;e
;;; Exception: parse-program returns just the AST after asserting
;;; that all tokens are consumed.

;; <Program> ::= <StmtList>
(define (parse-program tokens)
  (let-values ([(stmts remaining) (parse-stmt-list tokens)])
    (unless (null? remaining)
      (error 'syntax-error
             (format "Unexpected token after end of program: ~a ('~a')"
                     (token-type (car remaining))
                     (token-value (car remaining)))))
    (cons 'program stmts)))


;; <StmtList> ::= <Statement> { <Statement> }
;; Stops at END, ELSE, or EOF.
(define (parse-stmt-list tokens)
  (let loop ([toks tokens] [stmts '()])
    (let ([tt (peek-type toks)])
      (if (or (eq? tt 'END-TOK)
              (eq? tt 'ELSE-TOK)
              (eq? tt 'EOF))
          (values (reverse stmts) toks)
          (let-values ([(stmt rest) (parse-statement toks)])
            (loop rest (cons stmt stmts)))))))


;; <Statement> ::= <AssignStmt> | <IfStmt> | <WhileStmt> | <PrintStmt>
(define (parse-statement tokens)
  (match (peek-type tokens)
    ['IF-TOK    (parse-if-stmt    tokens)]
    ['WHILE-TOK (parse-while-stmt tokens)]
    ['PRINT-TOK (parse-print-stmt tokens)]
    ['ID-TOK    (parse-assign-stmt tokens)]
    [other
     (error 'syntax-error
            (format "Expected a statement, found ~a ('~a')"
                    other (peek-value tokens)))]))


;; <AssignStmt> ::= <ID> ":=" <Expression> ";"
(define (parse-assign-stmt tokens)
  (let* ([id-name (peek-value tokens)]
         [toks1   (consume tokens 'ID-TOK)]
         [toks2   (consume toks1  'ASSIGN-TOK)])
    (let-values ([(expr rest) (parse-expression toks2)])
      (values `(assign ,id-name ,expr)
              (consume rest 'SEMI-TOK)))))


;; <IfStmt> ::= "IF" <Comparison> "THEN" <StmtList> ["ELSE" <StmtList>] "END"
(define (parse-if-stmt tokens)
  (let-values ([(cmp toks2) (parse-comparison (consume tokens 'IF-TOK))])
    (let-values ([(then-body toks4) (parse-stmt-list (consume toks2 'THEN-TOK))])
      (if (eq? (peek-type toks4) 'ELSE-TOK)
          (let-values ([(else-body toks6) (parse-stmt-list (consume toks4 'ELSE-TOK))])
            (values `(if ,cmp (then ,@then-body) (else ,@else-body))
                    (consume-end toks6)))
          (values `(if ,cmp (then ,@then-body))
                  (consume-end toks4))))))


;; <WhileStmt> ::= "WHILE" <Comparison> "DO" <StmtList> "END"
(define (parse-while-stmt tokens)
  (let-values ([(cmp toks2) (parse-comparison (consume tokens 'WHILE-TOK))])
    (let-values ([(body toks4) (parse-stmt-list (consume toks2 'DO-TOK))])
      (values `(while ,cmp ,@body) (consume-end toks4)))))


;; <PrintStmt> ::= "PRINT" <Expression> ";"
(define (parse-print-stmt tokens)
  (let-values ([(expr rest) (parse-expression (consume tokens 'PRINT-TOK))])
    (values `(print ,expr) (consume rest 'SEMI-TOK))))


;; <Comparison> ::= <Expression> <RelOp> <Expression>
(define (parse-comparison tokens)
  (let-values ([(left toks1) (parse-expression tokens)])
    (unless (eq? (peek-type toks1) 'RELOP-TOK)
      (error 'syntax-error
             (format "Expected relational operator, found ~a ('~a')"
                     (peek-type toks1) (peek-value toks1))))
    (let* ([op    (peek-value toks1)]
           [toks2 (consume toks1 'RELOP-TOK)])
      (let-values ([(right toks3) (parse-expression toks2)])
        (values `(,(relop->symbol op) ,left ,right) toks3)))))

(define (relop->symbol op)
  (match op
    ["="  'eq]  ["!=" 'neq]
    [">"  'gt]  [">=" 'gte]
    ["<"  'lt]  ["<=" 'lte]
    [_ (error 'syntax-error (format "'~a' is not a valid relational operator" op))]))


;; <Expression> ::= <Term> { ("+" | "-") <Term> }   (left-associative)
(define (parse-expression tokens)
  (let-values ([(left toks) (parse-term tokens)])
    (let loop ([left left] [toks toks])
      (if (eq? (peek-type toks) 'ADDOP-TOK)
          (let* ([op-str (peek-value toks)]
                 [toks1  (consume toks 'ADDOP-TOK)])
            (let-values ([(right toks2) (parse-term toks1)])
              (loop `(,(string->symbol op-str) ,left ,right) toks2)))
          (values left toks)))))


;; <Term> ::= <Factor> { ("*" | "/") <Factor> }   (left-associative)
(define (parse-term tokens)
  (let-values ([(left toks) (parse-factor tokens)])
    (let loop ([left left] [toks toks])
      (if (eq? (peek-type toks) 'MULOP-TOK)
          (let* ([op-str (peek-value toks)]
                 [toks1  (consume toks 'MULOP-TOK)])
            (let-values ([(right toks2) (parse-factor toks1)])
              (loop `(,(string->symbol op-str) ,left ,right) toks2)))
          (values left toks)))))


;; <Factor> ::= <ID> | <INT> | <FP> | "(" <Expression> ")"
(define (parse-factor tokens)
  (match (peek-type tokens)
    ['ID-TOK
     (values (string->symbol (peek-value tokens)) (consume tokens 'ID-TOK))]
    ['INT-TOK
     (values (peek-value tokens) (consume tokens 'INT-TOK))]
    ['FP-TOK
     (values (peek-value tokens) (consume tokens 'FP-TOK))]
    ['LPAREN-TOK
     (let-values ([(expr toks1) (parse-expression (consume tokens 'LPAREN-TOK))])
       (values expr (consume toks1 'RPAREN-TOK)))]
    [other
     (error 'syntax-error
            (format "Expected a number, identifier, or '(' — found ~a ('~a')"
                    other (peek-value tokens)))]))

;;;=============================================================== SECTION 6 — TOP-LEVEL ENTRY POINTS

;; run-parser : String -> AST
(define (run-parser source)
  (parse-program (tokenize (strip-comments source))))

;; run-file : Path-String -> AST
(define (run-file filepath)
  (run-parser (file->string filepath)))

;;;=============================================================== SECTION 7 — TEST RUNNER

;;cosmetics
(define DIVIDER (make-string 60 #\-))