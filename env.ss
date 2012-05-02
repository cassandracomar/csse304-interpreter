(load "chez-init.ss")
(load "common.ss")
(load "functions.ss")

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
			 [empty-env-record () (extend-env sym new-val (empty-env))]
			 [extended-env-record (symbol value environment)
					      (if (eqv? sym symbol)
						  (extend-env sym new-val environment)
						  (extend-env symbol value (g environment)))]))]]
      (g old-env))))

(define modify-env-value-car!
  (lambda [sym new-val old-env]
    (letrec [[g (lambda [env]
		  (cases environment env
			 [empty-env-record () (extend-env sym new-val (empty-env))]
			 [extended-env-record (symbol value environment)
					      (if (eqv? sym symbol)
						  (extend-env sym (cons new-value (cdr value))
							      environment)
						  (extend-env symbol value (g environment)))]))]]
      (g old-env))))

(define modify-env-value-cdr!
  (lambda [sym new-val old-env]
    (letrec [[g (lambda [env]
		  (cases environment env
			 [empty-env-record () (extend-env sym new-val (empty-env))]
			 [extended-env-record (symbol value environment)
					      (if (eqv? sym symbol)
						  (extend-env sym (cons (car value) new-value)
							      environment)
						  (extend-env symbol value (g environment)))]))]]
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
	   [empty-env-record ()
			     (eopl:error 'apply-env "No binding for ~s" sym)]
	   [extended-env-record (symbol value env)
				(if (eq? symbol sym)
				    value
				    (apply-env env sym))])))

(define init-env
  (lambda ()
    (extend-env* (list 'map    'apply 'assq 'assv 'append 'else 'any 'all 'eval)
		 (list 'smap-1 sapply sassq sassv sappend  #t    any  all  eval)
     (extend-env* (list 'list? 'car 'cdr 'cadr 'cdar 'null? 'procedure? 'eq? 'set-car! 'set-cdr!)
		  (list  list?  car  cdr  cadr  cdar  null?  procedure?  eq?  set-car!  set-cdr!)
		  (extend-env* (list 'list 'vector 'vector? '+ '- '< '= '/ '* 'cons 'not 'void) 
			       (list  list  vector  vector?  +  -  <  =  /  *  cons  not  void) 
			       (empty-env))))))


