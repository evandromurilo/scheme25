;; exercicio do simply scheme, parece muito util

(define (compose f g)
  (lambda (x) (f (g x))))

(define second (compose first bf))
