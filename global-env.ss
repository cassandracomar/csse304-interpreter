(load "common.ss")
(load "functions.ss")

(define global-env '())

(define extend-global-env
  (lambda [sym val]
    (set! global-env (cons (list sym val) global-env))))

(define extend-global-env*
  (lambda [syms vals]
    (map extend-global-env syms vals)))

(define init-global-env
  (lambda ()
    (extend-global-env* (list 'map    'apply 'assq 'assv 'append 'else 'any 'all 'eval 'pair?)
			(list 'smap-1 sapply sassq sassv sappend  #t    any  all  eval  pair?)) 
    (extend-global-env* (list 'list? 'car 'cdr 'cadr 'cdar 'null? 'procedure? 'eq? 'set-car! 'set-cdr!)
			(list  list?  car  cdr  cadr  cdar  null?  procedure?  eq?  set-car!  set-cdr!))
    (extend-global-env* (list 'list 'vector 'vector? '+ '- '< '> '= '/ '* 'cons 'not 'void 'display) 
			(list  list  vector  vector?  +  -  <  >  =  /  *  cons  not  void  display))))

(init-global-env)

(define apply-global-env
  (lambda [sym]
    (let [[val (assv sym global-env)]]
      (if val
	  (cadr val)
	  (eopl:error 'apply-global-env "No binding for symbol ~s" sym)))))


;; shadowing the old binding should always be sufficient
;; I can't think of a case where this isn't true.
;; assv always returns the top level binding, so applying the modified environment
;; will always give the correct answer.
(define modify-global-env! extend-global-env)
;; This is **NOT** ideal. A set! on the same global variable over and over again
;; will cause the interpreter to consume memory, even though it shouldn't.

(define modify-global-env-car!
  (lambda [sym new-val]
    (set-car! (apply-global-env sym) new-val)))

(define modify-global-env-cdr!
  (lambda [sym new-val]
    (set-cdr! (apply-global-env sym) 1)))

;; Recursion should work by default.

;;RESET
(define reset-global-env
  (lambda ()
    (begin
      (set! global-env '())
      (init-global-env))))
