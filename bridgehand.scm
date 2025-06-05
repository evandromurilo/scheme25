;; projeto do simply scheme

(define (rank cr)
  (butfirst cr))

(define all-suits `shcd)

(define (suit cr)
  (first cr))

(define (make-ranker s)
  (lambda (cr) (equal? (suit cr) s)))

(define (card-val cr)
  (let ((r (rank cr)))
    (cond ((equal? r `a) 4)
	  ((equal? r `k) 3)
	  ((equal? r `q) 2)
	  ((equal? r `j) 1)
	  (else 0))))

(define (high-card-points hand)
  (accumulate + (every card-val hand)))

(define (count-suit s hand)
  (count (keep (make-ranker s) hand)))

(define (hand-suiter hand)
  (lambda (s) (count-suit s hand)))

(define (suit-counts hand)
  (every (hand-suiter hand) all-suits))

(define (suit-dist-points c)
  (cond ((= c 0) 3)
	((= c 1) 2)
	((= c 2) 1)
	(else 0)))
	 

(define (hand-dist-points hand)
  (accumulate + (every suit-dist-points (suit-counts hand))))

(define (bridge-val hand)
  (+ (high-card-points hand)
     (hand-dist-points hand)))
     
