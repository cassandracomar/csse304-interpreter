(load "common.ss")

(define atom?
  (lambda (datum)
    (any (lambda (pred) (pred datum))
	 (list number? boolean? string? vector? symbol? null?))))

(define-syntax let-cps
  (syntax-rules ()
    [(_ ((v1 e1) ...) b1 b2 ... cont)
     (cps-transform '((lambda (v1 ...) b1 b2 ...) e1 ...) cont)]
    [(_ name ((v1 e1) ...) b1 b2 ... cont)
     (cps-transform '(letrec [[name (lambda (v1 ...) b1 b2 ...)]]
		       (name e1 ...)) cont)]))

(define-syntax let*-cps
  (syntax-rules ()
    [(_ ((v1 e1)) b1 b2 ... cont)
     (cps-transform '((lambda (v1) b1 b2 ...) e1) cont)]
    [(_ ((v1 e1) (v2 e2) ...) b1 b2 ... cont)
     (cps-transform `((lambda (v1) ,(let*-cps ((v2 e2) ...) id)) e1) cont)]))

(define-syntax letrec-cps
  (syntax-rules ()
    [(_ ((var init) ...) b1 ... cont)
     (cps-transform '(begin
		       (define var init)
		       ...
		       (begin b1 ...)) cont)]))

(define-syntax cond-cps
  (syntax-rules (else)
    [(_ cont)
     (cont (void))]
    [(_ (else en) cont)
     (cont en)]
    [(_ (c1 e1) (c2 e2) ... cont)
     (if-cps c1 e1 (cond-cps (c2 e2) ... cont) cont)]
    [(_ (c1 e1) (c2 e2) ... (else en) cont)
     (if-cps c1 e1 (cond-cps (c2 e2) ... (else en) cont) cont)]))

(define-syntax case-cps
  (syntax-rules (case else)
    [(_ sym cont) (cont void)]
    [(_ sym (else e2) cont) (cps-transform e2 cont)]
    [(_ sym ((v11 v12 ...) b11 b12 ...) ((v21 v22 ...) b21 b22 ...) ... cont)
     (if-cps (any (lambda (x) (eqv? sym x)) (quote (v11 v12 ...)))
	 (begin-cps b11 b12 ... cont)
	 (case-cps sym ((v21 v22 ...) b21 b22 ...) ... cont))]
    [(_ sym ((v11 v12 ...) b11 b12 ...) ((v21 v22 ...) b21 b22 ...) ... (else en) cont)
     (if (any (lambda (x) (eqv? sym x)) (quote (v11 v12 ...)))
	 (begin-cps b11 b12 ... cont)
	 (case-cps sym ((v21 v22 ...) b21 b22 ...) ... (else en) cont))]))

(define-syntax or-cps
  (syntax-rules ()
    [(_ cont) (cont #f)]
    [(_ () cont) (cont #f)]
    [(_  e1 e2 ... cont)
     (cps-transform e1 (lambda (v)
			 (if v
			     (cont v)
			     (or-cps e2 ... cont))))]))

(define-syntax and-cps
  (syntax-rules (and)
    [(_ cont) (cont #t)]
    [(_  () cont) (cont #t)]
    [(_ e1 e2 cont)
     (cps-transform
      e1 (lambda (v1) (if v1
			  (cps-transform v2 (lambda (v2) (cont v2)))
			  (cont v1))))]
    [(_ e1 e2 ... cont)
     (cps-transform
      e1 (lambda (v1) (if v1
			  (and-cps e2 ... cont)
			  (cont v1))))]))


(define-syntax if-cps
  (syntax-rules ()
    [(_ test-exp true-exp false-exp cont)
     (cps-transform true-exp
		    (lambda (v)
		      (if v
			  (cps-transform true-exp cont)
			  (cps-transform false-exp cont))))]
    [(_ test-exp true-exp cont)
     (cps-transform true-exp (lambda (v) (when v (cps-transform true-exp cont))))]))

(define-syntax begin-cps
  (syntax-rules ()
    [(_ e1 cont)
     (cps-transform e1 cont)]
    [(_ e1 e2 ... cont)
     (cps-transform e1 (lambda (v) (begin-cps e2 ... cont)))]))

(define-syntax set!-cps
  (syntax-rules ()
    [(_ sym val cont)
     (cps-transform val (lambda (v) (set! sym val)
				(cont 'void)))]))

(define-syntax define-cps ;;internal define only!
  (syntax-rules ()
    [(_ sym val cont)
     (cps-transform val (lambda (v)
			  (define sym v)
			  (cont 'void)))]))

(define-syntax sequence-cps
  (syntax-rules ()
    [(_ b1 cont)
     '(cps-transform b1 cont)]
    [(_ b1 b2 ... cont)
     `(cps-transform b1 (lambda (v) ,(sequence-cps b2 ... cont)))]))

(define-syntax lambda-cps
  (syntax-rules ()
    [(_ vars b1 ... cont)
     (let [[k (gensym)]]
       `(cont (lambda ,(append (quote vars) (list k))
		,(eval `(sequence-cps b1 ... ,k)))))]))

;;assumes proc is already a cps function if it's not primitive!
;;define functions with def-cps to automatically transform new functions into cps style
(define-syntax app-cps
  (syntax-rules ()
    [(_ proc a1 a2 ... cont)
     (let [[tproc (assv (quote proc) transformed-prims)]]
       (cps-terms (list a1 a2 ...)
		  (lambda (v) (cont (apply (if (null? tproc) proc tproc) v)))))]))

(define cps-terms
  (lambda (args k)
    (if (null? args)
	(k '())
	(cps-terms (cdr args)
		   (lambda (v1)
		     (cps-transform (car args)
				    (lambda (v2) (k (pappend v1 v2)))))))))

(define-syntax cps-transform
  (syntax-rules ()
    [(_ datum cont)
     (if (atom? datum) (cont datum)
	 (case (car datum)
	   ((quote) (eval `(cont ,@(cdr datum))))
	   ((if) (eval `(if-cps ,@(cdr datum) cont)))
	   ((begin) (eval `(begin-cps ,@(cdr datum) cont)))
	   ((set!) (eval `(set!-cps ,@(cdr datum) cont)))
	   ((define) (eval `(define-cps ,@(cdr datum) cont)))
	   ((lambda) (eval `(lambda-cps ,@(cdr datum) cont)))
	   ((let) (eval `(let-cps ,@(cdr datum) cont)))
	   ((let*) (eval `(let*-cps ,@(cdr datum) cont)))
	   ((letrec) (eval `(letrec-cps ,@(cdr datum) cont)))
	   ((cond) (eval `(cond-cps ,@(cdr datum) cont)))
	   ((case) (eval `(case-cps ,@(cdr datum) cont)))
	   ((or) (eval `(or-cps ,@(cdr datum) cont)))
	   ((and) (eval `(and-cps ,@(cdr datum) cont)))
	   (else (eval `(app-cps ,@datum cont)))))]))

(define primitives
  '(and or
    + - * / % > < <= >= >> <<
    bitwise-or bitwise-and
    type
    number?
    string?
    symbol?
    key?
    boolean?
    null?
    list?
    vector?
    dict?
    function?
    literal?
    str
    symbol->key
    key->symbol
    string->key
    key->string
    string->symbol
    symbol->string
    _emptylst
    list
    cons
    car
    cdr
    cadr
    cddr
    cdar
    caddr
    cdddr
    cadar
    cddar
    caadr
    cdadr
    list-ref
    length
    list-append
    _list-append
    list-find
    map
    for-each
    fold
    reverse
    vector->list
    make-vector
    vector
    vector-ref
    vector-put!
    vector-concat
    vector-slice
    vector-push!
    vector-find
    vector-length
    list->vector
    vector-map
    vector-for-each
    vector-fold
    dict
    dict-put!
    dict-ref
    dict-map
    dict-merge
    dict->vector
    dict->list
    keys
    vals
    zip
    not
    ==
    =
    eq?
    equal?
    print
    println
    pp
    %inspect-non-sequence
    %recur-protect
    %space
    inspect
    apply
    trampoline-result?
    trampoline
    %gensym-base
    gensym-fresh
    gensym
    cps-trampoline
    cps-jump
    cps-halt
    RegExp
    s.match
    fs.readFileSync
    throw
    parseInt
    parseFloat))

(define make-prim-cps
  (lambda [f]
    (lambda args
      (let* [[rev (reverse args)]
	     [cont (car rev)]
	     [args (reverse (cdr rev))]]
	(cont (f args))))))

(define transformed-prims
  (map (lambda (prim tprim) `(,prim ,tprim)) primitives (map make-prim-cps primitives)))

(define-syntax def-cps
  (syntax-rules ()
    [(_ sym val)
     (define sym (cps-transform val id))]))
