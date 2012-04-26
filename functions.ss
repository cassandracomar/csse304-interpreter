(load "common.ss")

(define smap-1
  (lambda [f ls]
    (if (null? ls)
	'()
	(join (f (car ls)) (smap-1 f (cdr ls)) ))))

(define smap
  (lambda args
    (let [[f (car args)]
	  [ls (cdr args)]]
      (if (any null? ls)
	  '()
	  (join (sapply f (smap-1 car ls)) (smap f (smap-1 cdr ls)))))))

(define sapply
  (lambda [f ls]
    (eval (join f ls))))
