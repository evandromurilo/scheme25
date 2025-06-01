;; vi num exemplo do simply scheme uma versao do apply-to-itself e achei
;; divertida a idéia

(define (apply-to-itself f) (lambda (x) (f x x)))
(define (twice f) (lambda (x) (f (f x))))

(define square (apply-to-itself *))
(define double (apply-to-itself +))
(define quadruple (twice double))

