;; a palindrome function without reversing words or sentences (simply scheme)

(define (flatten sent)
  (if (empty? sent)
      (word)
      (word (first sent) (flatten (bf sent)))))

(define (palindrome sent)
  (define (helper wr)
    (cond ((< (count wr) 2)
	   #t)
	  ((equal? (first wr) (last wr))
	   (helper (bl (bf wr))))
	  (else
	   #f)))
  (helper (flatten sent)))
    
    
