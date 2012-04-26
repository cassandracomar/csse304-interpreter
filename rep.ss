(load "chez-init.ss")
(load "env.ss")
(load "parser.ss")
(load "interpreter.ss")

(define (rl) (load "int.ss"))

(define (rep)
  (begin
    (display "--> ")
    (let ([foo (read)])
      (when (not (equal? foo '(exit)))
      (begin (write (interpret foo))
         (newline)
         (rep))))))
