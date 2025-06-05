;; exercicio do simply scheme, achei muito legal
;; consiste em substituir os numeros da primeira sentenca pelas palavras da segunda
;; (unnabrev `(i 4 you so 7 my 8) `(there is no need for so much love))
;; > `(i need you so much my love)

(define (unnabrev sa sb)
  (every (lambda (wrd) (if (number? wrd)
			   (pos wrd sb)
			   wrd))
	 sa))

(define (pos n large)
  (first ((repeated bf (- n 1)) large)))

 
