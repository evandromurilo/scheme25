;; exercicio do simply scheme
;; gostei da implementacao
;; os indices sao inclusivos

(define (subword wr si ei)
  (take-first (skip-first wr (- si 1))
	      (- ei si -1)))

(define (skip-first wr n)
  ((repeated bf n) wr))

(define (skip-last wr n)
  ((repeated bl n) wr))

;; e` mais facil definir subword em termos de take-first do que skip-last
(define (take-first wr n)
  (skip-last wr (- (count wr) n)))


