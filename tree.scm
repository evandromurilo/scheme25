(define make-node cons)
(define datum car)
(define children cdr)

(define (leaf datum)
  (make-node datum '()))

(define (leaf? node)
  (null? (children node)))

(define (cities name-list)
  (map leaf name-list))

(define world-tree
  (make-node
   'world
   (list (make-node
	  'italy
	  (cities '(venezia riomaggiore firenze roma)))
	 (make-node
	  '(united states)
	  (list (make-node
		 'california
		 (cities '(berkeley (san francisco) gilroy)))
		(make-node
		 'massachusetts
		 (cities '(cambridge amherst sudbury)))))
	 (make-node 'zimbabwe (cities '(harare hwange)))
	 (make-node 'china (cities '(beijing shanghai guagzhou suzhou)))
	 (make-node
	  '(great britain)
	  (list
	   (make-node 'england (cities '(liverpool)))
	   (make-node 'scotland (cities '(edinburgh glasgow (gretna green))))
	   (make-node 'wales (cities '(abergavenny)))))
	 (make-node
	  'australia
	  (list
	   (make-node 'victoria (cities '(melbourne)))
	   (make-node '(new south wales) (cities '(sydney)))
	   (make-node 'queensland (cities '(cairns (port douglas))))))
	 (make-node 'honduras (cities '(tegucigalpa)))
	 (make-node
	  'brasil
	  (list
	   (make-node 'rondonia (cities '((ji parana) cacoal ariquemes (porto velho) buritis)))
	   (make-node '(sao paulo) (cities '((sao paulo) guarulhos (santo andre)))))))))

(define (count-leaves tree)
  (if (leaf? tree)
      1
      (reduce + (map count-leaves (children tree)))))
		      
;; alternative definition with mutual recursion
(define (count-leaves tree)
  (if (leaf? tree)
      1
      (count-leaves-in-forest (children tree))))

(define (count-leaves-in-forest forest)
  (if (null? forest)
      0
      (+ (count-leaves (car forest))
	 (count-leaves-in-forest (cdr forest)))))

(define (count-nodes tree)
  (if (leaf? tree)
      1
      (+ 1 (count-nodes-in-forest (children tree)))))

(define (count-nodes-in-forest forest)
  (if (null? forest)
      0
      (+ (count-nodes (car forest))
	 (count-nodes-in-forest (cdr forest)))))

(define (in-tree? place tree)
  (if (equal? (datum tree) place)
      #t
      (in-forest? place (children tree))))

(define (in-forest? place forest)
  (if (null? forest)
      #f
      (or (in-tree? place (car forest))
	  (in-forest? place (cdr forest)))))

(define (locate place tree)
  (if (equal? (datum tree) place)
      (list place)
      (let ((location (locate-in-forest place (children tree))))
	(if location
	    (cons (datum tree) location)
	    #f))))

(define (locate-in-forest place forest)
  (if (null? forest)
      #f
      (or (locate place (car forest))
	  (locate-in-forest place (cdr forest)))))

(define (max a b)
  (if (> b a)
      b
      a))

(define (forest-depth forest)
  (if (null? forest)
      0
      (max (depth (car forest))
	   (forest-depth (cdr forest)))))

(define (depth tree)
  (if (leaf? tree)
      1
      (+ 1 (forest-depth (children tree)))))
      
(define (prune tree)
  (if (leaf? tree)
      #f
      (make-node (datum tree)
		 (prune-forest (children tree)))))

(define (prune-forest forest)
  (if (null? forest)
      '()
      (if (null? (children (car forest)))
	  (prune-forest (cdr forest))
	  (cons (prune (car forest))
		(prune-forest (cdr forest))))))
