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

