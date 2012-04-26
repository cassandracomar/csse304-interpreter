(load "common.ss")

(define-datatype expression expression?
  (lit-exp
   (value scheme-value?))
  (var-exp
   (id symbol?))
  (lambda-exp
   (ids (list-of symbol?))
   (bodies (list-of expression?)))
  (lambda-list-exp
   (id symbol?)
   (bodies (list-of expression?)))
  (lambda-dotted-exp
   (ids (list-of symbol?))
   (term symbol?)
   (bodies (list-of expression?)))
  (if-exp
   (conditional expression?)
   (if-true expression?)
   (if-false expression?))
  (letrec-exp
   (syms (list-of symbols?))
   (vals (list-of scheme-value?))
   (exps (list-of expression?)))
  (set!-exp
   (sym symbol?)
   (set-exp expression?))
  (app-exp
   (exps (list-of expression?)))
  (void-exp))

(define cond-exp? 
  (lambda [v] (and (pair? v) (= (length v) 2))))
(define scheme-value? (lambda (v) #t))

(define equals?
  (lambda [x y]
    (if (= (length x) (length y))
	(eval `(and ,@(map eqv? x y)))
	#f)))

(define-syntax expand-let
  (syntax-rules (let letrec)
    [(_ (let ((v1 e1) ...) b1 b2 ...))
     (quote ((lambda (v1 ...) b1 b2 ...) e1 ...))]
    [(_ ((v1 e1) ...) b1 b2 ...)
     (quote ((lambda (v1 ...) b1 b2 ...) e1 ...))]
    [(_ (letrec ((v1 e1) ...) b1 b2 ...))
     (quote ((lambda (v1 ...) b1 b2 ...) e1 ...))]))

(define-syntax expand-let*
  (syntax-rules (let*)
    [(_ () b1 b2 ...) (expand-let () b1 b2 ...)]
    [(_ (let* () b1 b2 ...)) (expand-let () b1 b2 ...)]
    [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
     (let [[exp (expand-let* ([i2 e2] ...) b1 b2 ...)]]
       (eval `(expand-let ([i1 e1]) ,exp)))]
    [(_ (let* ((i1 e1) (i2 e2) ...) b1 b2 ...))
     (let [[exp (expand-let* ([i2 e2] ...) b1 b2 ...)]]
       (eval `(expand-let ([i1 e1]) ,exp)))]))

(define-syntax expand-cond
  (syntax-rules (cond else)
    [(_ (cond))
     '(void)]
    [(_ (cond (else en)))
     en]
    [(_ (cond (c1 e1) (c2 e2) ...))
     (let [[exp (expand-cond (cond (c2 e2) ...))]]
       `(if c1 e1 ,exp))]
    [(_ (cond (c1 e1) (c2 e2) ... (else en)))
     (let [[exp (expand-cond (cond (c2 e2) ... (else en)))]]
       `(if c1 e1 ,exp))]))

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
	  [(number? datum) (lit-exp datum)]
	  [(boolean? datum) (lit-exp datum)]
	  [(string? datum) (lit-exp datum)]
	  [(vector? datum) (lit-exp datum)]
	  [(pair? datum)
	   (cond
	    [(eqv? (car datum) 'void) (void-exp)]
	    [(eqv? (car datum) 'quote)
	     (lit-exp (cadr datum))]
	    [(eqv? (car datum) 'begin)
	     (parse-expression `((lambda [] ,@(cdr datum))))]
	    [(eqv? (car datum) 'lambda)
	     (cond ((symbol? datum) )
	      (lambda-exp (cadr datum)
			  (map parse-expression (cddr datum))))]
	    [(eqv? (car datum) 'if)
	     (cond [(= (length datum) 4)
		    (if-exp (parse-expression (cadr datum))
			    (parse-expression (caddr datum))
			    (parse-expression (cadddr  datum)))]
		   [(= (length datum) 3)
		    (if-exp (parse-expression (cadr datum))
			    (parse-expression (caddr datum))
			    (void-exp))]
		   [else (eopl:error 'parse-expression
			     "Invalid concrete syntax ~s" datum)])]
	    [(eqv? (car datum) 'set!)
	     (set!-exp (cadr datum) (parse-expression (caddr datum)))]
	    [(eqv? (car datum) 'let)
	     (parse-expression (eval `(expand-let ,datum)))]
	    [(eqv? (car datum) 'let*)
	     (parse-expression (eval `(expand-let* ,datum)))]
	    [(eqv? (car datum) 'letrec)
	     (let [[syms (map car (cadr datum))]
		   [vals (map cadr (cadr datum))]]
	       (letrec-exp (eval `(expand-let ,datum))))]
	    [(eqv? (car datum) 'cond)
	     (parse-expression (eval `(expand-cond ,datum)))]
	    [else (app-exp (map parse-expression datum))])]
	  [else (eopl:error 'parse-expression
			    "Invalid concrete syntax ~s" datum)])))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
	   [var-exp (id) id]
	   [lit-exp (val) val]
	   [lambda-exp (id body)
		       (list 'lambda
			     (list id)
			     (unparse-expression body))]
	   [app-exp (operator operand)
		    (list (unparse-expression operator)
			  (unparse-expression operand))]
	   [if-exp (condition if-true if-false)
		    (list 'if
			  (unparse-expression condition)
			  (unparse-expression if-true)
			  (unparse-expression if-false))])))


