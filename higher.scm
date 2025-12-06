(define (left-accumulate op lst)
  (if (null? (cdr lst))
      (car lst)
      (op (left-accumulate op (cdr lst)) (car lst))))

(define (true-for-all? op lst)
  (if (null? lst)
      #t
      (and (op (car lst)) (true-for-all? op (cdr lst)))))

(define (true-for-any-pair? op lst)
  (if (null? (cdr lst))
      #f
      (or
       (op (car lst) (cadr lst))
       (true-for-any-pair? op (cdr lst)))))

(define (true-for-all-pairs? op lst)
  (if (null? (cdr lst))
      #t
      (and (op (car lst) (cadr lst))
	   (true-for-all-pairs? op (cdr lst)))))

(define (true-for-all-pairs? op lst)
  (not (true-for-any-pair? (lambda (a b) (not (op a b))) lst)))
