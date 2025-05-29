;; exercício do simply scheme para validar datas

(define (in-range? x l u)
  (and (>= x l)
       (<= x u)))

(define (divisible? small big)
  (= (remainder small big) 0))

(define (feb-has-29-days? y)
  (if (divisible? y 100)
      (divisible? y 400)
      (divisible? y 4)))

(define (last-day-of-feb y)
  (if (feb-has-29-days? y)
      29
      28))

(define (last-day-of-month m y)
  (cond ((= m 1) 31)
	((= m 2) (last-day-of-feb y))
	((= m 3) 31)
	((= m 4) 30)
	((= m 5) 31)
	((= m 6) 30)
	((= m 7) 31)
	((= m 8) 31)
	((= m 9) 30)
	((= m 10) 31)
	((= m 11) 30)
	((= m 12) 31)
	(else 0)))
	 
(define (valid-date? m d y)
  (and (in-range? m 1 12)
       (>= y 0)
       (in-range? d 1 (last-day-of-month m y))))
       
		
