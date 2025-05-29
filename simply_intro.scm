;; introduction examples of simply scheme

(define (acronym phrase)
  (accumulate word (every first phrase)))

(define (acronym phrase)
  (accumulate word (every first (keep real-word? phrase))))

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (butfirst wd) (first wd)))))

(define (rotate wd) ;; nice
  (word (butfirst wd) (first wd)))

(define (choices menu)
  (if (null? menu)
      '(())
      (let ((smaller (choices (cdr menu))))
	(reduce append
		(map (lambda (item) (prepend-every item smaller))
		     (car menu))))))

(define (prepend-every item lst)
  (map (lambda (choice) (se item choice)) lst))

(define (combinations size set)
  (cond ((= size 0) '(()))
	((empty? set) '())
	(else (append (prepend-every (first set)
				     (combinations (- size 1)
						   (butfirst set)))
		      (combinations size (butfirst set))))))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
      
				     
