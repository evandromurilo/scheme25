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

(define (to-binary dec)
  ; acha o maior multiplo de 2 que nao ultrapassa dec
  (define (find-scale dec scale)
    (cond ((> dec scale)
	   (find-scale dec (* scale 2)))
	  ((= dec scale)
	   scale)
	  (else
	   (/ scale 2))))

  (define (helper dec scale)
    (cond ((< scale 1)
	   (word))
	  ((>= dec scale)
	   (word 1 (helper (- dec scale) (/ scale 2))))
	  (else
	   (word 0 (helper dec (/ scale 2))))))

  (helper dec (find-scale dec 1)))

