;; exercicio do simply scheme, exagera uma frase
;; ex: (exagerar `(pesquei um bom peixe de 10 cm))
;;
;; deixei simples porque na verdade acho que vou conseguir melhorar quando
;; estudar paradigmns of artificial inteligence

(define (exagerar st)
  (every exagerar-palavra st))

(define (exagerar-palavra wr)
  (cond ((number? wr) (* wr 2))
	((equal? wr `bom) `incrivel)
	((equal? wr `ruim) `pessimo)
	((equal? wr `legal) `fenomenal)
	(else wr)))
	  
