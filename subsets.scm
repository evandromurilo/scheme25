;; find all the subsets of a word

(define (subset wr)
  (cond ((empty? wr)
	 (sentence wr))
	(else
	 (let ((x (subset (bf wr))))
	   (sentence (append-to-all (first wr) x) x)))))

(define (append-to-all small big)
  (cond ((empty? big)
	 (sentence))
	(else
	 (sentence (word small (first big))
		   (append-to-all small (bf big))))))
