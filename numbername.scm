;; project Spelling Names of Huge Numbers from simply scheme
;; example (spell-num 1000529) -> (one million five hundred twenty nine)
(define scale '(() thousand million billion trillion quadrillion quintillion sextillion septillion octillion nonillion decillion))

(define (spell-num num)
  (define (helper scale nums)
    (if (empty? nums)
	(list)
	(let ((num (reverse-term (first nums))))
	  (if (= (remove-zeroes num) 0)
	      (helper (bf scale) (bf nums))
	      (sentence (helper (bf scale) (bf nums))
			(spell-hundred num)
			(first scale))))))
  (let ((gr (groups-of 3 (reverse-term num))))
    (helper scale gr)))

(define (between x a b)
  (and (>= x a) (<= x b)))

(define (spell-tens num)
  (case num
    ((1) 'ten)
    ((2) 'twenty)
    ((3) 'thirty)
    ((4) 'forty)
    ((5) 'fifty)
    ((6) 'sixty)
    ((7) 'seventy)
    ((8) 'eighty)
    ((9) 'ninety)))

(define (spell-teen num)
  (case num
    ((10) 'ten)
    ((11) 'eleven)
    ((12) 'twelve)
    ((13) 'thirteen)
    ((14) 'fourteen)
    ((15) 'fifteen)
    ((16) 'sixteen)
    ((17) 'seventeen)
    ((18) 'eighteen)
    ((19) 'nineteen)))

(define (spell-digit num)
  (case num
    ((0) 'zero)
    ((1) 'one)
    ((2) 'two)
    ((3) 'three)
    ((4) 'four)
    ((5) 'five)
    ((6) 'six)
    ((7) 'seven)
    ((8) 'eight)
    ((9) 'nine)))

(define (remove-zeroes num)
  (cond ((empty? num) 0)
	((equal? (first num) 0)
	 (remove-zeroes (bf num)))
	(else num)))

(define (spell-hundred num)
  (let ((num (remove-zeroes num)))
    (cond ((>= num 100)
	   (if (= (remove-zeroes (bf num)) 0)
	       (sentence (spell-hundred (first num)) 'hundred)
	       (sentence
		(spell-hundred (first num)) 'hundred
		(spell-hundred (bf num)))))
	  ((between num 20 99)
	   (if (= (bf num) 0)
	       (spell-tens (first num))
	       (sentence
		(spell-tens (first num))
		(spell-hundred (bf num)))))
	  ((between num 10 19)
	   (spell-teen num))
	  (else
	   (spell-digit num)))))
	 
(define (reverse-term term)
  (let ((f (if (sentence? term) sentence word)))
    (if (empty? term)
	term
	(f (reverse-term (bf term)) (first term)))))

(define (take n term)
  (cond ((empty? term) (word))
	((= n 0) (word))
	(else (word (first term) (take (- n 1) (bf term))))))

(define (bf-n n term)
  (cond ((empty? term) (word))
	((= n 0) term)
	(else (bf-n (- n 1) (bf term)))))

(define (groups-of size term)
  (cond ((empty? term) (sentence))
	(else (sentence (take size term) (groups-of size (bf-n size term))))))

  
    

   




