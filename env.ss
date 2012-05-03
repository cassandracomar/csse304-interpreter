(load "chez-init.ss")
(load "common.ss")
(load "functions.ss")
(load "global-env.ss")

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (sym symbol?)   
   (val scheme-value?)
   (env environment?)])

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (sym val env)
    (extended-env-record sym val env)))

(define extend-env*
  (lambda (syms vals env)
    (foldr (lambda [pair curr-env] 
	     (extend-env (car pair) (cadr pair) curr-env))
	   env
	   (map (lambda [sym val]
		  (list sym val)) syms vals))))

;;; Replaces the top level binding for a symbol with new-val
(define modify-env-value!
  (lambda [sym new-val old-env]
    (letrec [[g (lambda [env]
		  (cases environment env
			 [empty-env-record ()
					   (modify-global-env! sym new-val)]
			 [extended-env-record (symbol value environment)
					      (if (eqv? sym symbol)
						  (list-set! env 2 new-val)
						  (g environment))]))]]
      (g old-env))))

(define modify-env-value-car!
  (lambda [sym new-val old-env]
    (letrec [[g (lambda [env]
		  (cases environment env
			 [empty-env-record () (modify-global-env-car! sym new-val)]
			 [extended-env-record (symbol value environment)
					      (if (eqv? sym symbol)
						  (list-set! env 2 (cons new-val (cdr value)))
						  (g environment))]))]]
      (g old-env))))

(define modify-env-value-cdr!
  (lambda [sym new-val old-env]
    (letrec [[g (lambda [env]
		  (cases environment env
			 [empty-env-record () (modify-global-env-cdr! sym new-val)]
			 [extended-env-record (symbol value environment)
					      (if (eqv? sym symbol)
						  (list-set! env 2 (cons (car value) new-val))
						  (g environment))]))]]
      (g old-env))))

;;; Replaces the top level binding for the provided closure
(define modify-env!
  (lambda [proc old-env new-env]
    (letrec [[g (lambda [env]
		  (cases environment env
			 [empty-env-record ()
					   (eopl:error 'modify-env! "No binding for proc ~s" sym)]
			 [extended-env-record 
			  (symbol val environment)
			  (if (eqv? proc symbol)
			      (cases 
			       closure val
			       [closure-record 
				(ids bodies env)
				(extend-env proc (make-closure ids bodies new-env) environment)])
			      (extend-env symbol val (g environment)))]))]]
      (g old-env))))

(define extend-env*-recur
  (lambda (syms vals env)
    (let [[new-env (extend-env* syms vals env)]]
      (map (lambda (sym val)
	     (when (proc? val)
		   (modify-env! sym val env new-env))) syms vals)
      new-env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
	   [empty-env-record () ;check the global environment
			     (apply-global-env sym)]
	   [extended-env-record (symbol value env)
				(if (eqv? symbol sym)
				    value
				    (apply-env env sym))])))

(define init-env
  (lambda ()
    (empty-env-record)))


