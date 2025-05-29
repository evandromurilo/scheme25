;; exercício do simply scheme para converter entre 24h e am/pm
;; (load "simply/simply.scm")

(define (hour tm)
  (first tm))

(define (meridiem tm)
  (first (bf tm)))

(define (am? tm)
  (equal? (meridiem tm) 'am))

(define (pm? tm)
  (equal? (meridiem tm) 'pm))

(define (noon? tm)
  (equal? '(12 pm) tm))

(define (midnight? tm)
  (equal? '(12 am) tm))

(define (european-time tm)
  (cond ((noon? tm) 12)
	((midnight? tm) 0)
	((am? tm) (hour tm))
	(else (+ 12 (hour tm)))))

(define (american-time hour)
  (cond ((= hour 0) '(12 am))
	((= hour 12) '(12 pm))
	((> hour 12) (se (- hour 12) 'pm))
	(else (se hour 'am))))
