(define (get-song n)
  (let ((port (open-input-file "songs")))
    (skip-songs (- n 1) port)
    (let ((answer (read port)))
      (close-input-port port)
      answer)))

(define (skip-songs n port)
  (if (= n 0)
      'done
      (begin (read port)
	     (skip-songs (- n 1) port))))

(define (print-file name)
  (let ((port (open-input-file name)))
    (print-file-helper port)
    (close-input-port port)
    'done))

(define (print-file-helper port)
  (let ((stuff (read-line port)))
    (if (eof-object? stuff)
	'done
	(begin (show-line stuff)
	       (print-file-helper port)))))

(define (file-map fn inname outname)
  (let ((inport (open-input-file inname))
	(outport (open-output-file outname)))
    (file-map-helper fn inport outport)
    (close-input-port inport)
    (close-output-port outport)
    'done))

(define (file-map-helper fn inport outport)
  (let ((line (read inport)))
    (if (eof-object? line)
	'done
	(begin (show (fn line) outport)
	       (file-map-helper fn inport outport)))))

(define (lastfirst name)
  (se (last name) (bl name)))

(define (concatenate innames outname)
  (let ((outport (open-output-file outname)))
    (concatenate-helper innames outport)
    (close-output-port outport)))

(define (concatenate-helper innames outport)
  (if (null? innames)
      'done
      (let ((inport (open-input-file (car innames))))
	(copy-file inport outport)
	(close-input-port inport)
	(concatenate-helper (cdr innames) outport))))

(define (copy-file inport outport)
  (let ((term (read inport)))
    (if (eof-object? term)
	'done
	(begin
	  (display term outport)
	  (copy-file inport outport)))))
    
(define (count-lines inname)
  (let ((inport (open-input-file inname)))
    (let ((tot (count-lines-helper inport)))
      (close-input-port inport)
      tot)))

(define (count-lines-helper port)
  (if (eof-object? (read-line port))
      0
      (+ 1 (count-lines-helper port))))

(define (wordcount inname)
  (let ((inport (open-input-file inname)))
    (let ((tot (wordcount-helper inport)))
      (close-input-port inport)
      tot)))

(define (wordcount-helper inport)
  (let ((line (read-line inport)))
    (if (eof-object? line)
	0
	(+ (count line) (wordcount-helper inport)))))

(define (dedup inname outname)
  (let ((inport (open-input-file inname))
	(outport (open-output-file outname)))
    (dedup-helper inport outport 'no-line)
    (close-input-port inport)
    (close-output-port outport)))

(define (dedup-helper inport outport prev)
  (let ((line (read-line inport)))
    (if (eof-object? line)
	'done
	(begin
	  (if (not (equal? line prev))
	      (show-line line outport))
	  (dedup-helper inport outport line)))))

(define (lookup inname word)
  (let ((inport (open-input-file inname)))
    (lookup-helper inport word)
    (close-input-port inport)
    'done))

(define (in-line? line word)
  (if (empty? line)
      #f
      (if (equal? (car line) word)
	  #t
	  (in-line (cdr line) word))))

(define (lookup-helper inport word)
  (let ((line (read-line inport)))
    (if (eof-object? line)
	'done
	(begin
	  (when (in-line? line word)
	    (show-line line))
	  (lookup-helper inport word)))))
	    
  
