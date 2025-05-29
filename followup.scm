;; esse exercício eu passei em php em sala de aula no dia 23/05/2025
(define (sequence-of? x y)
  (= (abs (- x y)) 1))

;; por causa do sequence-of?, a lista pode ondular, exemplo? 4 5 6 7 6 7 6 5
(define (in-sequence? ns)
  (if (or (nil? ns) (nil? (cdr ns))) ;; zero or 1-element list
      #t
      (and (sequence-of? (car ns) (cadr ns))
	   (in-sequence? (cdr ns)))))

;; mesma estrutura
(define (double-of? x y)
  (= (* x 2) y))

(define (all-doubles? ns)
    (if (or (nil? ns) (nil? (cdr ns)))
      #t
      (and (double-of? (car ns) (cadr ns))
	   (all-doubles? (cdr ns)))))

;; podemos abstrair
(define (follow-up fun lst)
  (if (or (nil? lst) (nil? (cdr lst)))
      #t
      (and (fun (car lst) (cadr lst))
	   (follow-up fun (cdr lst)))))

(define (in-sequence? ns)
  (follow-up sequence-of? ns))

(define (all-doubles? ns)
  (follow-up double-of? ns))
