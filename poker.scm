;; [x] royal flush
;; [ ] straight flush
;; [x] four of a kind
;; [ ] full house
;; [ ] flush
;; [x] straight
;; [x] three of a kind
;; [ ] two pair
;; [x] pair
;; [x] nothing


(define (poker-value hand)
  (let* ((hand (sort-hand hand))
	 (c-rank (count-by-rank hand)))
    (cond ((royal-flush? hand)
	   '(royal flush))
	   ((straight? hand)
	   '(straight))
	  ((four-of-a-kind? c-rank)
	   '(four of a kind))
	  ((three-of-a-kind? c-rank)
	   '(three of a kind))
	  ((pair? c-rank)
	   '(pair))
	  (else '(nein)))))

(define (sort-hand hand)
  (define (remove-card card hand)
    (cond ((empty? hand)
	   (sentence))
	  ((equal? card (first hand))
	   (bf hand))
	  (else
	   (sentence (first hand) (remove-card card (bf hand))))))
  (define (earliest-card hand)
    (cond ((= (count hand) 1)
	   (first hand))
	  ((> (rank-value (first hand)) (rank-value (first (bf hand))))
	   (earliest-card (bf hand)))
	  (else
	   (earliest-card (sentence (first hand) (bf (bf hand)))))))
  (if (empty? hand)
      (sentence)
      (let ((f (earliest-card hand)))
	(sentence f
		  (sort-hand (remove-card f hand))))))
	
(define (pair? c-rank)
  (member? 'two c-rank))

(define (three-of-a-kind? c-rank)
  (member? 'three c-rank))

(define (four-of-a-kind? c-rank)
  (member? 'four c-rank))

(define (royal-flush? hand)
  (and (single-suit? hand)
       (equal? (rank-of (first hand)) 'a)
       (equal? (rank-of (first (bf hand))) 10)
       (in-sequence? (bf hand))))

(define (single-suit? hand)
  (or (< (count hand) 2)
      (and (equal? (suit-of (first hand))
		   (suit-of (first (bf hand))))
	   (single-suit? (bf hand)))))

(define (straight? hand)
  (if (equal? (rank-of (first hand)) 'a)
      (or (in-sequence? hand)
	  (in-sequence? (sentence (bf hand) (first hand))))
      (in-sequence? hand)))

(define (in-sequence? hand)
  (or (empty? hand)
      (= (count hand) 1)
      (and (follows? (first hand) (first (bf hand)))
	   (in-sequence? (bf hand)))))

(define (rank-of card)
  (bf card))

(define (suit-of card)
  (first card))

(define (follows? a-card b-card)
  (or (and (equal? (rank-of a-card) 'k)
	    (equal? (rank-of b-card) 'a))
      (= (rank-value b-card)
	 (+ (rank-value a-card) 1))))

(define (rank-value card)
  (let ((rank (rank-of card)))
    (cond ((equal? rank 'a) 0)
	  ((equal? rank 'j) 11)
	  ((equal? rank 'q) 12)
	  ((equal? rank 'k) 13)
	  (else rank))))

(define (spell-num num)
  (cond ((= num 1) 'one)
	((= num 2) 'two)
	((= num 3) 'three)
	((= num 4) 'four)
	((= num 5) 'five)
	(else 'out-of-bounds)))

;; hand must be sorted by rank beforehand
(define (count-by-rank hand)
  (define (helper hand rank tot)
    (cond ((empty? hand)
	   (if (> tot 0)
	       (sentence (spell-num tot) rank)
	       (sentence)))
	  ((equal? (rank-of (first hand)) rank)
	   (helper (bf hand) rank (+ 1 tot)))
	  (else
	   (sentence (spell-num tot) rank
		     (helper (bf hand) (rank-of (first hand)) 1)))))
  (if (empty? hand)
      (sentence)
      (helper (bf hand) (rank-of (first hand)) 1)))
