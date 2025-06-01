;; o desafio é achar alguma frase que faca sentido com o vogalwords

(define (letterwords letter sent)
  (keep (lambda (x) (member? letter x)) sent))

(define (vogalwords sent)
  (every (lambda (x) (letterwords x sent)) `aeiou))
