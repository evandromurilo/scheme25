;; vi no simply scheme um exemplo de converter um único caracter romano para o
;; valor decimal correspondente, e pareceu divertido fazer um programa completo

;; depende do simply.scm para o tipo "word" e "sentence"
;; (load "simply/simply.scm")

(define (single-roman-to-num r)
  (cond ((equal? r `i) 1)
	((equal? r `v) 5)
	((equal? r `x) 10)
	((equal? r `l) 50)
	((equal? r `c) 100)
	((equal? r `d) 500)
	((equal? r `m) 1000)))
      
(define (many-roman-to-num r)
  (if (empty? r)
      '()
      (se (single-roman-to-num (first r)) (many-roman-to-num (bf r)))))

(define (arabic r)
  (roman-sum (many-roman-to-num r) (single-roman-to-num (last r))))

(define (roman-sum ds prev)
  (cond ((empty? ds) 0)
	((< (last ds) prev) (- (roman-sum (bl ds) (last ds)) (last ds)))
	(else (+ (last ds) (roman-sum (bl ds) (last ds))))))

;; esse faz o inverso, converte de arábico para romano, sem usar ainda a forma subtrativa (quer dizer que 4 vira iiii em vez de iv)
(define (roman n)
  (cond ((>= n 1000) (word `m (roman (- n 1000))))
	((>= n 500) (word `d (roman (- n 500))))
	((>= n 100) (word `c (roman (- n 100))))
	((>= n 50) (word `l (roman (- n 50))))
	((>= n 10) (word `x (roman (- n 10))))
	((>= n 5) (word `v (roman (- n 5))))
	((>= n 1) (word `i (roman (- n 1))))
	(else (word)))) ;; não tem zero em romanos
