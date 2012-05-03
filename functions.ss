(load "common.ss")

(define alist?
  (lambda [ls]
    (and (list? ls) (all paired-exp? ls))))

(define smap-1
  (lambda [f ls]
    (if (null? ls)
	'()
	(join (f (car ls)) (smap-1 f (cdr ls))))))

(define smap
  (lambda args
    (let [[f (car args)]
	  [ls (cdr args)]]
      (if (any null? ls)
	  '()
	  (let [[v (sapply f (smap-1 car ls))]
		[rest (smap f (smap-1 cdr ls))]]
	    (print v)
	    (print rest)
	    (eval `(cons ,(sapply f (smap-1 car ls)) ,@rest)))))))

(define sapply
  (lambda [f ls]
    (eval (cons f ls))))

(define sappend
  (lambda [ls1 ls2]
    (cond [(not (list? ls1)) (eopl:error 'append "Error in: ~s -- expected proper list" ls1)]
	  [(pair? ls1) (let [[l (car ls1)]
			     [t (cdr ls1)]]
			 (cons l (sappend t ls2)))]
	  [else ls2])))

(define sass-general
  (lambda [eqv-func]
   (lambda [obj al]
     (if (alist? al)
	 (let [[matches (filter (lambda (x) (eqv-func obj (car x))) al)]]
	   (if (null? matches)
	       #f
	       (car matches)))
	 (eopl:error 'assq "Error in: ~s -- expected an alist" al)))))

(define sassq (sass-general eq?))
(define sassv (sass-general eqv?))
(define sassoc (sass-general equal?))
