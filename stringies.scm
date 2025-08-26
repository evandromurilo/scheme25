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

(define (crescendo l wr)
  (if (empty? wr)
      (sentence l)
      (sentence l
		(crescendo (word l (first wr)) (bf wr)))))

;; returns all the substrings of wr
(define (substrings wr)
  (if (empty? wr)
      (sentence)
      (sentence
       (crescendo (first wr) (bf wr))
       (substrings (bf wr)))))

(define (starts-with? wr a)
  (cond ((empty? a)
	 #t)
	((empty? wr)
	 #f)
	((equal? (first wr) (first a))
	 (starts-with? (bf wr) (bf a)))
	(else
	 #f)))

(define (substring? a wr)
  (if (empty? wr)
      #f
      (or (starts-with? wr a)
	  (substring? a (bf wr)))))
