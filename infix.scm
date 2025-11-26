(define (make-node datum children)
  (cons datum children))

(define (make-leaf datum)
  (cons datum '()))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (add-child node child)
  (make-node (datum node) (append (children node) (list child))))

(define (parse exp)
  (define (precedes? a b)
    (and (or (equal? a '*)
	     (equal? a '/))
	 (or (equal? b '+)
	     (equal? b '-))))
  (define (sub prev exp)
    (cond ((null? exp) prev)
	  ((null? (cdr exp))
	   (add-child prev (make-leaf (car exp))))
	  (else (let ((op (cadr exp)))
		  (if (precedes? (datum prev) op)
		      (make-node op (list (add-child prev (make-leaf (car exp))) (parse (cddr exp))))
		      (add-child prev (parse exp)))))))
  (cond ((null? exp) '())
	((null? (cdr exp))
	 (make-node (car exp) '()))
	(else
	 (sub (make-node (cadr exp)
			 (list (make-leaf (car exp))))
	      (cddr exp)))))
  
(parse '(4 + 3))	 
(parse '(4 + 3 * 7 - 5 / (3 + 4) + 6))
