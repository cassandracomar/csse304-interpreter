
(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)]
	   [initial-environment (init-env)]
	   [result (eval-expression parse-tree initial-environment)])
      result)))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
	   [var-exp (id) (apply-env env id)]
	   [lit-exp (val) val]
	   [if-exp (conditional if-true if-false)
		   (if (eval-expression conditional env)
		       (eval-expression if-true env)
		       (eval-expression if-false env))]
	   [lambda-exp (ids bodies)
		       (make-closure ids bodies env)]
	   [set!-exp (sym exp)
		     (modify-env-value! sym (eval-expression exp env) env)]
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
	       [closure-record (ids bodies env)
			       (let [[env (extend-env* ids args env)]] 
				 (last (map (lambda [b] 
					     (eval-expression b env)) 
					   bodies)))])
	(apply proc args))))
