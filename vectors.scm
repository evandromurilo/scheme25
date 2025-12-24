(define (sum-vector vec)
  (sum-vector-helper vec (- (vector-length vec) 1)))

(define (sum-vector-helper vec index)
  (if (< index 0)
      0
      (+ (vector-ref vec index) (sum-vector-helper vec (- index 1)))))

(define (vector-fill! vec val)
  (vector-fill-helper vec val (- (vector-length vec) 1)))

(define (vector-fill-helper vec val index)
  (if (< index 0)
      vec
      (begin
	(vector-set! vec index val)
	(vector-fill-helper vec val (- index 1)))))

(define (vector-append a-vec b-vec)
  (let ((new (make-vector (+ (vector-length a-vec) (vector-length b-vec)))))
    (vector-copy! new a-vec 0)
    (vector-copy! new b-vec (vector-length a-vec))
    new))

(define (vector-copy! vec old-vec from-index)
  (vector-copy-helper vec old-vec from-index 0 (vector-length old-vec)))

(define (vector-copy-helper vec old-vec from-index index total)
  (if (or (equal? total 0))
      'done
      (begin
	(vector-set! vec (+ from-index index) (vector-ref old-vec index))
	(vector-copy-helper vec old-vec from-index (+ index 1) (- total 1)))))
			
(define (vector->list vec)
  (vector->list-helper vec 0))

(define (vector->list-helper vec index)
  (if (equal? index (vector-length vec))
      (list)
      (cons (vector-ref vec index)
	    (vector->list-helper vec (+ index 1)))))

(define (vector-map fun vec)
  (vector-map-helper (make-vector (vector-length vec)) fun vec (- (vector-length vec) 1)))

(define (vector-map-helper new fun vec index)
  (if (< index 0)
      new
      (begin
	(vector-set! new index (fun (vector-ref vec index)))
	(vector-map-helper new fun vec (- index 1)))))

(define (vector-map! fun vec)
  (vector-map-helper vec fun vec (- (vector-length vec) 1)))

(define (vector-filter fun vec)
  (let* ((new (make-vector (vector-length vec)))
	 (size (vector-filter-helper new fun vec (- (vector-length vec) 1) 0))
	 (newest (make-vector size)))
    (vector-copy-helper newest new 0 0 size)
    newest))

(define (vector-filter-helper new fun vec index total)
  (if (< index 0)
      total
      (if (fun (vector-ref vec index))
	  (begin
	    (vector-set! new total (vector-ref vec index))
	    (vector-filter-helper new fun vec (- index 1) (+ total 1)))
	  (vector-filter-helper new fun vec (- index 1) total))))

(define *tables* (make-vector 28 0))

(define *menu* '((potstickers 3.24) (wor-won-ton 4.56) (egg-rolls 2.25) (shin-shin-special-prawns 5.35)))

(define (assoc-ref assoc key)
  (if (empty? assoc)
      #f
      (if (equal? (caar assoc) key)
	  (car assoc)
	  (assoc-ref (cdr assoc) key))))

(define (item-price item)
  (cadr (assoc-ref *menu* item)))

(define (order number item)
  (vector-set! *tables* number (+ (item-price item) (vector-ref *tables* number))))

(define (bill number)
  (let ((price (vector-ref *tables* number)))
    (vector-set! *tables* number 0)
    price))
    
  
(define (vector-sort! vec)
  (vector-sort-helper vec 0))

(define (vector-sort-helper vec index)
  (if (equal? index (vector-length vec))
      vec
      (let ((min-index (vector-min-index vec index)))
	(unless (equal? min-index index)
	  (vector-swap! vec index min-index))
	(vector-sort-helper vec (+ index 1)))))

(define (vector-min-index vec index)
  (vector-min-index-helper vec (+ index 1) index))

(define (vector-min-index-helper vec index min)
  (if (equal? (vector-length vec) index)
      min
      (if (< (vector-ref vec index) (vector-ref vec min))
	  (vector-min-index-helper vec (+ index 1) index)
	  (vector-min-index-helper vec (+ index 1) min))))

(define (vector-swap! vec a b)
  (let ((temp (vector-ref vec a)))
    (vector-set! vec a (vector-ref vec b))
    (vector-set! vec b temp)))

(define (do-times fun n)
  (do-times-helper fun n 0))

(define (do-times-helper fun max cur)
  (if (equal? cur max)
      'done
      (begin
	(fun cur)
	(do-times-helper fun max (+ cur 1)))))

(define (make-matrix x y)
  (let ((vec (make-vector x)))
    (do-times (lambda (n) (vector-set! vec n (make-vector y))) x)
    vec))

(define (matrix-ref matrix x y)
  (vector-ref (vector-ref matrix x) y))

(define (matrix-set! matrix x y val)
  (vector-set! (vector-ref matrix x) y val))
