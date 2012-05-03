(load "common.ss")

(define-datatype expression expression?
  (def-exp
    (symbol symbol?)
    (exp expression?))
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
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (exps (list-of expression?)))
  (set!-exp
   (sym symbol?)
   (set-exp expression?))
  (set-car-exp!
   (sym symbol?)
   (set-car!-exp expression?))
  (set-cdr-exp!
   (sym symbol?)
   (set-cdr!-exp expression?))
  (app-exp
   (exps (list-of expression?)))
  (void-exp))

(define paired-exp? 
  (lambda [v] (and (pair? v) (= (length v) 2))))
(define scheme-value? (lambda (v) #t))

(define equals?
  (lambda [x y]
    (if (= (length x) (length y))
	(eval `(and ,@(map eqv? x y)))
	#f)))

(define-syntax for
  (syntax-rules (:)
    [(_ (init-exp : test-exp : up1 up2 ...) body)
     (begin init-exp
	    (let forloop []  
	      (when test-exp (begin body
				    (begin up1 up2 ...)
				    (forloop)))))]))

(define-syntax return-first
  (syntax-rules ()
    [(_ e1 e2 ...)
     (let [[val e1]]
       (begin e2 ... val))]))

(define-syntax expand-let
  (syntax-rules (let)
    [(_ ((v1 e1) ...) b1 b2 ...)
     (quote ((lambda (v1 ...) b1 b2 ...) e1 ...))]
    [(_ (let name ((v1 e1) ...) b1 b2 ...))
     `(let [[name (lambda () ,(expand-let ((v1 e1) ...) b1 b2 ...))]]
	(name))]
    [(_ (let ((v1 e1) ...) b1 b2 ...))
     (quote ((lambda (v1 ...) b1 b2 ...) e1 ...))]))

(define-syntax expand-let*
  (syntax-rules (let*)
    [(_ () b1 b2 ...) (expand-let () b1 b2 ...)]
    [(_ (let* () b1 b2 ...)) (expand-let () b1 b2 ...)]
    [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
     (eval `(expand-let ([i1 e1]) ,(expand-let* ([i2 e2] ...) b1 b2 ...)))]
    [(_ (let* ((i1 e1) (i2 e2) ...) b1 b2 ...))
     (eval `(expand-let ([i1 e1]) ,(expand-let* ([i2 e2] ...) b1 b2 ...)))]))

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

(define-syntax expand-or
  (syntax-rules (or)
    [(_ (or)) #f]
    [(_ (or ())) #f]
    [(_ (or e1 e2 ...))  `(let [[v e1]] (if v v ,(expand-or (or e2 ...))))]))

(define-syntax expand-and
  (syntax-rules (and)
    [(_ (and)) #t]
    [(_ (and ())) #t]
    [(_ (and e1 e2)) '(let ([v e1]) (if v e2 v))]
     [(_ (and e1 e2 ...)) `(let [[v e1]] (if v ,(expand-and (and e2 ...)) v))]))

(define-syntax expand-case
  (syntax-rules (case else)
    [(_ (case sym)) '(void)]
    [(_ (case sym (else e2))) e2]
    [(_ (case sym ((v11 v12 ...) b11 b12 ...) ((v21 v22 ...) b21 b22 ...) ...))
     `(if (any (lambda (x) (eqv? sym x)) (quote (v11 v12 ...)))
	 (begin b11 b12 ...)
	 ,(expand-case (case sym ((v21 v22 ...) b21 b22 ...) ...)))]
    [(_ (case sym ((v11 v12 ...) b11 b12 ...) ((v21 v22 ...) b21 b22 ...) ... (else en)))
     `(if (any (lambda (x) (eqv? sym x)) (quote (v11 v12 ...)))
	 (begin b11 b12 ...)
	 ,(expand-case (case sym ((v21 v22 ...) b21 b22 ...) ... (else en))))]))

(define-syntax expand-while
  (syntax-rules (while)
    [(_ (while test-exp b1 b2 ...))
     '(let wloop []
	(if test-exp
	    (begin b1 b2 ... (wloop)) void))]))

(define-syntax expand-apply
  (syntax-rules (apply)
    [(_ (apply proc (quote (l1 l2 ...))))
     '(proc l1 l2 ...)]))

;; parse-expression doesn't make use of a separate syntax-expand function.
;; It instead calls the appropriate expansion directly. This prevents the need
;; to run through the abstract syntax tree multiple times.
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
	    [(eqv? (car datum) 'define) (def-exp (cadr datum) (parse-expression (caddr datum)))]
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
	    [(eqv? (car datum) 'set-car!)
	     (set-car-exp! (cadr datum) (parse-expression (caddr datum)))]
	    [(eqv? (car datum) 'set-car!)
	     (set-cdr-exp! (cadr datum) (parse-expression (caddr datum)))]
	    [(eqv? (car datum) 'let)
	     (parse-expression (eval `(expand-let ,datum)))]
	    [(eqv? (car datum) 'let*)
	     (parse-expression (eval `(expand-let* ,datum)))]
	    [(eqv? (car datum) 'letrec)
	     (let [[syms (map car (cadr datum))]
		   [vals (map parse-expression (map cadr (cadr datum)))]
		   [bodies (map parse-expression (cddr datum))]]
	       (letrec-exp syms vals bodies))]
	    [(eqv? (car datum) 'cond)
	     (parse-expression (eval `(expand-cond ,datum)))]
	    [(eqv? (car datum) 'or)
	     (parse-expression (eval `(expand-or ,datum)))]
	    [(eqv? (car datum) 'and)
	     (parse-expression (eval `(expand-and ,datum)))]
	    [(eqv? (car datum) 'while)
	     (parse-expression (eval `(expand-while ,datum)))]
	    [(eqv? (car datum) 'case)
	     (parse-expression (eval `(expand-case ,datum)))]
	    [(eqv? (car datum) 'apply)
	     (parse-expression (eval `(expand-apply ,datum)))]
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


