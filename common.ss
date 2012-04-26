(require srfi/1)
;(define foldl fold-left)
;(define foldr fold-right)
;(define last (lambda [xs] (cond ((null? xs) '()) ((null? (cdr xs)) (car xs)) (else (last (cdr xs))))))
;(define dotted-list? (lambda [ls] (cond ((null? xs) #f)	((null? (cdr ls)) #f) ((symbol? (cdr ls) #t)) (else (dotted-list? (cdr ls))))))
(define join (lambda [x y] (append (list x) y)))
(define joinl (lambda [x y] (cons x (list y))))

