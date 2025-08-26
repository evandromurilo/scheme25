(define (flatten sent)
  (if (empty? sent)
      (word)
      (word (first sent) (flatten (bf sent)))))

;; a palindrome function without reversing words or sentences
(define (palindrome sent)
  (define (helper wr)
    (cond ((< (count wr) 2)
	   #t)
	  ((equal? (first wr) (last wr))
	   (helper (bl (bf wr))))
	  (else
	   #f)))
  (helper (flatten sent)))

(define (interpolate l wr)
  (if (empty? wr)
      (sentence l)
      (sentence l
		(interpolate (word l (first wr)) (bf wr)))))

;; returns all the substrings of wr
(define (substrings wr)
  (if (empty? wr)
      (sentence)
      (sentence
       (interpolate (first wr) (bf wr))
       (substrings (bf wr)))))
