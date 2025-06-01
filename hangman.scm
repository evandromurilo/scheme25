;; futuramente vai ser um jogo da forca interativo

(define (mask wrd guesses)
  (let ((passthrough (word guesses `-)))
    (every (lambda (x) (if (member? x passthrough) x `_))
	   wrd)))
		 
(define (won? wrd guesses)
  (not (member? `_ (mask wrd guesses))))
