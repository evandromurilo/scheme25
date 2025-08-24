;; simple conversor from binary notation to decimal, depends on simply.scm

(define (from-binary bits)
  (if (empty? bits)
      0
      (+ (* (from-binary (bl bits)) 2)
	 (last bits))))

(define (from-binary binum)
  (define (helper rbinum scale)
    (cond ((empty? rbinum) 0)
	  (else (+ (* (first rbinum) scale)
	     (helper (bf rbinum) (* scale 2))))))
  (helper (reverse-term binum) 1))
