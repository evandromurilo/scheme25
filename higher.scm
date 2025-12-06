(define (left-accumulate op lst)
  (if (null? (cdr lst))
      (car lst)
      (op (left-accumulate op (cdr lst)) (car lst))))
