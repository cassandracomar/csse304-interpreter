
(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)]
	   [initial-environment (init-env)]
	   [result (eval-expression parse-tree initial-environment)])
      result)))

(define eval-top-level
  (lambda (exp env)
    (cases expression exp
	   [def-exp (sym exp)
	     (modify-global-env! sym (eval-expression exp env))]
	   [begin-exp (bodies)
		      (letrec [[env (extend-env* ids args old-env)]
			       [g (lambda [curr-env last-val bs]
				    (if (null? bs)
					last-val
					(let [[val (eval-top-level (car bs) curr-env)]]
					  (g curr-env val (cdr bs)))))]] 
			(g env (void) bodies))]
	   [var-exp (id) (apply-env env id)]
	   [lit-exp (val) val]
	   [if-exp (conditional if-true if-false)
		   (if (eval-top-level conditional env)
		       (eval-top-level if-true env)
		       (eval-top-level if-false env))]
	   [lambda-exp (ids bodies)
		       (make-closure ids bodies env)]
	   [set!-exp (sym exp)
		     (begin (modify-env-value! sym (eval-expression exp env) env) (void))]
	   [set-car-exp! (sym exp)
			 (begin (modify-env-value-car! sym (eval-expression exp env) env) #f)]
	   [set-cdr-exp! (sym exp)
			 (begin (modify-env-value-cdr! sym (eval-expression exp env env) (void)))]
	   [void-exp () (void)]
	   [app-exp (exps)
		    (let ([vals (map (lambda (x) (eval-top-level x env)) exps)])
		      (apply-proc (car vals) (cdr vals)))]
	   [else (eopl:error 'eval-expression "Received bad expression ~s" exp)])))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
	   [def-exp (sym exp)
	    ; (set! env (extend-env sym (eval-expression exp env) env))
	     (modify-global-env! sym (eval-expression exp env))
	     ]
	   [begin-exp (bodies)
		      (letrec [[g (lambda [curr-env last-val bs]
				    (if (null? bs)
					last-val
					(let [[val (eval-expression (car bs) curr-env)]]
					  (g curr-env val (cdr bs)))))]] 
			(g env (void) bodies))]
	   [var-exp (id) (apply-env env id)]
	   [lit-exp (val) val]
	   [if-exp (conditional if-true if-false)
		   (if (eval-expression conditional env)
		       (eval-expression if-true env)
		       (eval-expression if-false env))]
	   [lambda-exp (ids bodies)
		       (make-closure ids bodies env)]
	   [set!-exp (sym exp)
		     (begin (modify-env-value! sym (eval-expression exp env) env) (void))]
	   [set-car-exp! (sym exp)
			 (begin (modify-env-value-car! sym (eval-expression exp env) env) #f)]
	   [set-cdr-exp! (sym exp)
			 (begin (modify-env-value-cdr! sym (eval-expression exp env env) (void)))]
	   [void-exp () (void)]
	   [app-exp (exps)
		    (let ([vals (map (lambda (x) (eval-expression x env)) exps)])
		      (apply-proc (car vals) (cdr vals)))]
	   [else (eopl:error 'eval-expression "Received bad expression ~s" exp)])))


(define make-closure
  (lambda (ids body env)
    (closure-record ids body env)))

(define-datatype closure closure?
  [closure-record
   (ids (list-of symbol?))
   (bodies (list-of expression?))
   (env environment?)])

(define apply-proc
  (lambda (proc args)
    (if (closure? proc)
	(cases closure proc
	       [closure-record (ids bodies old-env)
			       (letrec [[env (extend-env* ids args old-env)]
					[g (lambda [curr-env last-val bs]
					     (if (null? bs)
						 last-val
						 (let [[val (eval-expression (car bs) curr-env)]]
						   (g curr-env val (cdr bs)))))]] 
				 (g env (void) bodies))])
	(apply proc args))))
