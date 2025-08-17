;; a very simple select sort for the sentences in simply scheme

(define (sort sent)
  (if (empty? sent)
      (sentence)
      (let ((head (earliest-word sent)))
	(sentence head (sort (remove-once head sent))))))

(define (remove-once wr sent)
  (cond ((empty? sent) (sentence))
	((equal? wr (first sent))
	 (bf sent))
	(else
	 (sentence (first sent) (remove-once wr (bf sent))))))

(define (earliest-word sent)
  (define (helper sent earliest)
    (cond ((empty? sent)
	   earliest)
	  ((before? (first sent) earliest)
	   (helper (bf sent) (first sent)))
	  (else
	   (helper (bf sent) earliest))))
  (if (empty? sent)
      (word)
      (helper (bf sent) (first sent))))
