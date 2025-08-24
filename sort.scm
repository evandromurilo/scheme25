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

;; now a merge sort

(define (merge-sort sent)
  (define (merge a b)
    (cond ((empty? a) b)
	  ((empty? b) a)
	  ((before? (first a) (first b))
	   (sentence (first a) (merge (bf a) b)))
	  (else
	   (sentence (first b) (merge (bf b) a)))))

  (define (one-half sent)
    (cond ((<= (count sent) 1)
	   sent)
	  (else
	   (sentence (first sent) (one-half (bf (bf sent)))))))

  (define (other-half sent)
    (cond ((<= (count sent) 1)
	   (sentence))
	  (else
	   (sentence (first (bf sent)) (other-half (bf (bf sent)))))))
      
    (cond ((<= (count sent) 1) sent)
	  (else (merge (merge-sort (one-half sent))
		       (merge-sort (other-half sent))))))
