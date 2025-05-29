;; calcula o gpa (grade point average) com base numa serie de notas estilo
;; americano (A+ A etc)
;;
;; exercicio do simply scheme

(define (base-grade grade)
  (let ((l (first grade)))
    (cond ((equal? l `A) 4)
	  ((equal? l `B) 3)
	  ((equal? l `C) 2)
	  ((equal? l `D) 1)
	  ;; normalmente nao tem o E
	  ((equal? l `F) 0)
	  (else 0))))

(define (grade-modifier grade)
  (cond ((member? `+ grade) 0.33)
	((member? `- grade) -0.33)
	(else 0)))

(define (point-grade grade)
  (+ (base-grade grade) (grade-modifier grade)))

(define (gpa st)
  (/ (accumulate + (every point-grade st)) (count st)))
      
