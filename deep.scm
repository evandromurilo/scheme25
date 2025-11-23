(define (deep-appearances wd structure)
  (if (word? structure)
      (if (equal? wd structure) 1 0)
      (reduce + (map (lambda (sub) (deep-appearances wd sub)) structure))))


;; alternative definition without higher-order functions
(define (deep-appearances wd structure)
  (if (word? structure)
      (if (equal? wd structure) 1 0)
      (if (null? structure) 0
	  (+ (deep-appearances wd (cdr structure))
	     (deep-appearances wd (car structure))))))

(define (my-append a b)
  (if (null? a)
      b
      (cons (car a) (my-append (cdr a) b))))

(define (before-in-list? l a b)
  (cond ((null? l) #f)
	((equal? (car l) a) #t)
	((equal? (car l) b) #f)
	(else (before-in-list? (cdr l) a b))))

(define (flatten l)
  (cond ((null? l) l)
	((list? (car l))
	 (append (flatten (car l))
		 (flatten (cdr l))))
	(else (cons (car l) (flatten (cdr l))))))

;; alternative definition without append
;; look at this beauty!
(define (flatten l)
  (cond ((null? l) l)
	((null? (car l))
	 (flatten (cdr l)))
	((list? (car l))
	 ;; here we pluck the first element of the inner list to the outside
	 (flatten (cons (caar l) (cons (cdar l) (cdr l)))))
	(else (cons (car l) (flatten (cdr l))))))

(define (deep-count lst)
  (cond ((null? lst) 0)
	((word? lst) 1)
	(else (+ (deep-count (car lst))
		 (deep-count (cdr lst))))))

(define (branch target lst)
  (if (null? target)
      lst
      (branch (cdr target) (item (car target) lst))))

;; (valid-infix? '(1 + 1 * (2 * 5) - 5 - (3 - (2 * 4))))
(define (valid-infix? lst)
  (define (operator? obj)
    (member obj '(+ - * /)))
  (define (operand? obj)
    (or (number? obj)
	(list? obj)))
  (define (valid-from-operator? lst)
    (or (null? lst)
	(and (operator? (car lst))
	     (valid-from-operand? (cdr lst)))))
  (define (valid-from-operand? lst)
    (cond ((null? lst)
	   #f)
	  ((number? (car lst))
	   (valid-from-operator? (cdr lst)))
	  ((list? (car lst))
	   (and (valid-from-operand? (car lst))
		(valid-from-operator? (cdr lst))))
	  (else #f)))
  
  (valid-from-operand? lst))


