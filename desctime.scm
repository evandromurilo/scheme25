;; exercício do simply list para descrever certa quantidade de segundos

(define (describe-time seconds)
  (cond ((< seconds 60) (thismany seconds 'second))
	((< seconds (* 60 60)) (thismany (/ seconds 60.0) 'minute))
	((< seconds (* 60 60 24)) (thismany (/ seconds (* 60.0 60)) 'hour))
	((< seconds (* 60 60 24 365)) (thismany (/ seconds (* 60.0 60 24)) 'day))
	((< seconds (* 60 60 24 365 10)) (thismany (/ seconds (* 60.0 60 24 365)) 'year))
	((< seconds (* 60 60 24 365 10 10)) (thismany (/ seconds (* 60.0 60 24 365 10)) 'decade))
	((< seconds (* 60 60 24 365 10 10 10)) (thismany (/ seconds (* 60.0 60 24 365 10 10)) 'century))
	(else (thismany (/ seconds (* 60.0 60 24 365 10 10 10)) 'millenium))))


(define (plural wrd)
  (cond ((and (equal? (last wrd) `y) (vowel? (last (bl wrd)))) (word wrd `s))
	((equal? (last wrd) `x) (word wrd `es))
	((equal? (last wrd) `y) (word (bl wrd) `ies))
	((and (equal? (last wrd) 'm) (equal? (last (bl wrd)) 'u)) (word (bl (bl wrd)) 'a))
	(else (word wrd `s))))      

(define (thismany n thing)
  (if (= 1 n)
      (se 1 thing)
      (se n (plural thing))))
