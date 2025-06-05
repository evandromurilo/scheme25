;; exercicio do simply scheme, consiste em transformar um procedure em outro que aceite numbers ou sentences

(define (aplize f)
  (lambda (x)
    (if (sentence? x)
	(every f x)
	(f x))))
