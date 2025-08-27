(define (number-letters num)
  (cond ((= num 2) '(a b c))
	((= num 3) '(d e f))
	((= num 4) '(g h i))
	((= num 5) '(j k l))
	((= num 6) '(m n o))
	((= num 7) '(p q r s))
	((= num 8) '(t u v))
	((= num 9) '(w x y z))
	(else '())))

(define (phone-spells num)
  (define (combinations ls combs)
    (cond ((empty? combs)
	   (sentence))
	  ((empty? ls)
	   (sentence))
	  (else
	   (sentence
	    (single-combinations (first ls) combs)
	    (combinations (bf ls) combs)))))

  (define (single-combinations l combs)
    (if (empty? combs)
	(sentence)
	(sentence
	 (word l (first combs))
	 (single-combinations l (bf combs)))))
  
  (cond ((empty? num)
	 (sentence))
	((= (count num) 1)
	 (number-letters num))
	(else
	 (combinations (number-letters (first num)) (phone-spells (bf num))))))
