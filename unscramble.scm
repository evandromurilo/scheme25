;; samples
;; $ (unscramble '(this is the roach the gladiator killed))
;; (this is the gladiator that killed the roach)
;; $ (unscramble '(this is the rat the cat the dog the boy the girl saw owned chased bit))
;; (this is the girl that saw the boy that owned the dog that chased the cat that bit the rat)
(define (unscramble sent)
  (cond ((empty? sent)
	 (sentence))
	((equal? (first sent) 'the)
	 (if (= (count sent) 2)
	     sent
	     (let ((verb (last sent))
		   (acusative (first (bf sent))))
	       (sentence (unscramble (bf (bf (bl sent)))) 'that verb 'the acusative))))
	(else
	 (sentence (first sent) (unscramble (bf sent))))))
