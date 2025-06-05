(define (mkeep pred sent)
  (accumulate sentence (every (lambda (x) (if (pred x) x `())) sent))) 
