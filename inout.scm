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
	  (in-line? (cdr line) word))))

(define (lookup-helper inport word)
  (let ((line (read-line inport)))
    (if (eof-object? line)
	'done
	(begin
	  (when (in-line? line word)
	    (show-line line))
	  (lookup-helper inport word)))))
	    
  
(define (with-inport inname fn)
  (let ((inport (open-input-file inname)))
    (let ((res (fn inport)))
      (close-input-port inport)
      res)))

(define (page inname)
  (with-inport inname (lambda (inport) (page-helper inport 24 ""))))

(define (page-helper inport cnt last)
  (if (equal? cnt 0)
      (begin
	(read)
	(show-line last)
	(page-helper inport 24 last))
      (let ((line (read-line inport)))
	(if (eof-object? line)
	    'done
	    (begin
	      (show-line line)
	      (page-helper inport (- cnt 1) line))))))

;; join database a with b on columns ai bi
(define (join aname bname ai bi outname)
  (let ((aport (open-input-file aname))
	(bport (open-input-file bname))
	(outport (open-output-file outname)))
    (join-helper aport bport ai bi outport (read aport) #f)
    (close-input-port aport)
    (close-input-port bport)
    (close-output-port outport)
    'done))

(define (at lst cnt)
  (if (equal? cnt 1)
      (car lst)
      (at (cdr lst) (- cnt 1))))

(define (skip-until inport i target)
  (let ((lst (read inport)))
    (if (eof-object? lst)
	#f
	(let ((key (at lst i)))
	  (if (or (equal? key target) (before? target key))
	      lst
	      (skip-until inport i target))))))

(define (join-helper aport bport ai bi outport a-row invert)
  (if (or (eof-object? a-row) (equal? #f a-row))
      'done
      (let* ((key (at a-row ai))
	     (b-row (skip-until bport bi key)))
	(if (equal? key (at b-row bi))
	    (begin
	      (if invert
		  (show (append b-row a-row) outport)
		  (show (append a-row b-row) outport))
	      (join-helper aport bport ai bi outport (read aport) invert))
	    (join-helper bport aport bi ai outport b-row (not invert))))))
