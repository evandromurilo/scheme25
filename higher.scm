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

(define (bubble-sort lst op)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((attempt (if (op (car lst) (cadr lst))
			 (cons (car lst) (bubble-sort (cdr lst) op))
			 (cons (cadr lst) (bubble-sort (cons (car lst) (cddr lst)) op)))))
	(if (equal? attempt lst)
	    attempt
	    (bubble-sort attempt op)))))

(define (deep-reduce op lst)
  (if (null? lst)
      (op)
      (op (if (list? (car lst))
	      (deep-reduce op (car lst))
	      (car lst))
	  (deep-reduce op (cdr lst)))))
		     
