(define (display-each lst)
  (if (null? lst)
      'done
      (begin
	(display (car lst))
	(if (not (null? (cdr lst)))
	    (display " "))
	(display-each (cdr lst)))))

(define (converse)
  (display "Hello, I'm the computer. What's your name? ")
  (let ((name (read-line)))
    (display "Hi, ")
    (display-each name)
    (display ". How are you? ")
    (read-line)
    (show "Glad to hear it")))

(define (name-table-helper names width)
  (if (null? names)
      'done
      (begin (display (align (cadar names) width))
	     (show (caar names))
	     (name-table-helper (cdr names) width))))

(define (longest-last-name names)
  (if (null? names)
      0
      (max (count (cadar names))
	   (longest-last-name (cdr names)))))
  
(define (name-table names)
  (name-table-helper names (+ 2 (longest-last-name names))))
