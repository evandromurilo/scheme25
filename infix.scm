(define (make-node datum children)
  (cons datum children))

(define (make-leaf datum)
  (cons datum '()))

(define (leaf? node)
  (empty? (children node)))

(define (branch? node)
  (not (leaf? node)))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (add-child node child)
  (make-node (datum node) (append (children node) (list child))))

(define (precedes? a b)
    (and (or (equal? a '*)
	     (equal? a '/))
	 (or (equal? b '+)
	     (equal? b '-))))

(define (operator? a)
  (or (equal? a '*)
      (equal? a '+)
      (equal? a '-)
      (equal? a '/)))

(define (parse exp)
  (parse-helper exp '() '()))

(define (parse-helper exp rators rands)
  (define (pop-operator)
    (make-node (car rators) (list (cadr rands) (car rands))))

  (define (pop-operator-and-recurse)
    (parse-helper
     exp
     (cdr rators)
     (cons (pop-operator) (cddr rands))))
  
  (cond ((null? exp)
	 (if (null? rators)
	     (car rands)
	     (pop-operator-and-recurse)))
	((operator? (car exp))
	 (if (and (not (null? rators)) (not (precedes? (car exp) (car rators))))
	     (pop-operator-and-recurse)
	     (parse-helper (cdr exp) (cons (car exp) rators) rands)))
	((number? (car exp))
	 (parse-helper (cdr exp) rators (cons (make-leaf (car exp)) rands)))
	(else (parse-helper (cdr exp) rators (cons (parse (car exp)) rands)))))
	
(define (parse-scheme exp)
  (define (parse-scheme-operand exp)
    (if (number? exp)
	(make-leaf exp)
	(parse-scheme exp)))
  (make-node (car exp)
	     (list
	      (parse-scheme-operand (cadr exp))
	      (parse-scheme-operand (caddr exp)))))
	     

(define (calc exp)
  (if (number? (datum exp))
      (datum exp)
      (apply (primitive-eval (datum exp))
	 (map calc (children exp)))))
  
(parse '(4 + 3))	 
(parse '(4 + 3 * 7 - 5 / (3 + 4) + 6))
