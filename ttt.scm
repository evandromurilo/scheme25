;; é um exemplo do simply scheme
;; o mais interessante é a idéia de transformar a representacao inicial do
;; tabuleiro em "triples", que sao as unidades relevantes
(define (ttt position me)
  (let ((triples (find-triples position)))
    (cond ((i-can-win? triples me))
	  ((opponent-can-win? triples me))
	  ((i-can-fork? triples me))
	  ((i-can-advance? triples me))
	  (else (best-free-square triples)))))

(define (find-triples position)
  (every (lambda (comb)
	   (substitute-triple comb position))
	 `(123 456 789 147 258 369 159 357)))

(define (substitute-triple combination position)
  (accumulate word
	      (every (lambda (square)
		       (substitute-letter square position))
		     combination)))

(define (substitute-letter square position)
  (if (equal? `_ (item square position))
      square
      (item square position)))

(define (i-can-win? triples me)
  (choose-win (keep (lambda (triple) (i-can-win-triple? triple me)) triples)))

(define (opponent-can-win? triples me)
  (i-can-win? triples (opponent me)))

(define (choose-win winning-triples)
  (if (empty? winning-triples)
      #f
      (keep number? (first winning-triples))))

(define (occurences small big)
  (count-keep (lambda (x) (equal? small x)) big))

(define (count-keep pred sent)
  (count (keep (lambda (x) (pred x)) sent)))

(define (opponent me)
  (if (equal? me `o) `x `o))

(define (i-can-win-triple? triple me)
  (and (= (occurences me triple) 2)
       (= (occurences (opponent me) triple) 0)))

(define (my-single? triple me)
  (and (= (occurences me triple) 1)
       (= (occurences (opponent me) triple) 0)))

(define (i-can-fork? triples me)
  (first-if-any (pivots triples me)))

(define (available-triples triples me)
  (keep (lambda (triple) (my-single? triple me)) triples))

(define (pivots triples me)
  (repeated-numbers (available-triples triples me)))

(define (repeated-numbers sent)
  (every first
	 (keep (lambda (wd) (>= (count wd) 2))
	       (sort-digits (accumulate word sent)))))

(define (extract-digit desired-digit wd)
  (keep (lambda (wd-digit) (equal? wd-digit desired-digit)) wd))

(define (sort-digits wd)
  (every (lambda (n) (extract-digit n wd))
	 `(1 2 3 4 5 6 7 8 9)))

(define (first-if-any sent)
  (if (empty? sent)
      #f
      (first sent)))

(define (i-can-advance? triples me)
  (best-move (keep (lambda (triple) (my-single? triple me)) triples)
	     triples
	     me))

(define (best-move my-triples all-triples me)
  (if (empty? my-triples)
      #f
      (best-square (first my-triples) all-triples me)))

(define (best-square my-triple triples me)
  (best-square-helper (pivots triples (opponent me))
		      (keep number? my-triple)))

(define (best-square-helper opponent-pivots pair)
  (if (member? (first pair) opponent-pivots)
      (first pair)
      (last pair)))

(define (best-free-square triples)
  (first-choice (accumulate word triples)
		`(5 1 3 7 9 2 4 6 8)))

(define (first-choice possibilities preferences)
  (first (keep (lambda (square) (member? square possibilities))
	       preferences)))

(define (already-won? triples me)
  (= (occurences (word me me me) triples) 1))

(define (tie-game? position) ;; to be called after already-won?
  (not (member? `_ position)))

(define (will-tie? triples) ;; check if the game is fated to tie
  (and (no-hope? triples `o)
       (no-hope? triples `x)))

(define (no-hope? triples me) ;; there is no hope if the opponent is in every triple!
  (empty? (keep (lambda (triple)
		  (= (occurences (opponent me) triple) 0))
		triples)))
