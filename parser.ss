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

(define-datatype temp-exp temp-exp?
  (let-exp
   (bindings (list-of paired-exp?))
   (bodies (list-of expression?)))
  (let*-exp
   (bindings (list-of paired-exp?))
   (bodies (list-of expression?)))
  (cond-exp
   (conditionals (list-of paired-exp?))
   (else-exp expression?))
  (and-exp
   (exps (list-of expression?)))
  (or-exp
   (exps (list-of expression?)))
  (case-exp
   (sym symbol?)
   (clauses (list-of paired-exp?))
   (else-exp expression?)))

(define paired-exp? 
  (lambda [v] (and (pair? v) (= (length v) 2))))
(define scheme-value? (lambda (v) #t))

(define equals?
  (lambda [x y]
    (if (= (length x) (length y))
	(eval `(and ,@(map eqv? x y)))
	#f)))

(define-syntax expand-syntax
  (syntax-rules (let-exp let*-exp cond-exp and-exp or-exp case-exp)
    [(_ (let-exp ((v1 e1) ... ) b1 b2 ...))
     (quote ((lambda (v1 ...) b1 b2 ...) e1 ...))]
    [(_ (let*-exp ((v1 e1) ...) b1 b2 ...))
     (expand-let* ((v1 e1) ...) b1 b2 ...)]
    [(_ (or-exp ())) #f]
    [(_ (or-exp (e1 e2 ...)))
     `(if e1 #t ,(expand-syntax (or-exp (e2 ...))))]
    [(_ (and-exp ())) #t]
    [(_ (and-exp (e1 e2 ...))) `(if e1 ,(expand-syntax (and-exp (e2 ...))) #f)]))

(define-syntax expand-let*
  (syntax-rules (let*)
    [(_ () b1 b2 ...) (expand-let () b1 b2 ...)]
    [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
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
	     (cond ((symbol? datum) (lambda-list-exp (cadr datum)
						     (map parse-expression (cddr datum))))
		   ((dotted-list? datum) (lambda-dotted-exp))
		   (else (lambda-exp (cadr datum)
				     (map parse-expression (cddr datum)))))]
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
	     (parse-expression (eval `(expand-syntax (let-exp ,(cadr datum) ,@(cddr datum)))))]
	    [(eqv? (car datum) 'let*)
	     (parse-expression (eval `(expand-let* ,datum)))]
	    [(eqv? (car datum) 'letrec)
	     (let [[syms (map car (cadr datum))]
		   [vals (map cadr (cadr datum))]
		   [bodies (cddr datum)]]
	       (letrec-exp syms vals bodies))]
	    [(eqv? (car datum) 'cond)
	     (parse-expression (eval `(expand-cond ,datum)))]
	    [(eqv? (car datum) 'or)
	     (parse-expression (eval `(expand-syntax (or-exp ,(cdr datum)))))]
	    [(eqv? (car datum) 'and)
	     (parse-expression (eval `(expand-syntax (and-exp ,(cdr datum)))))]
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


