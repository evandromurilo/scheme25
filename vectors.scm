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
  (vector-copy-helper vec old-vec from-index (- (vector-length old-vec) 1)))

(define (vector-copy-helper vec old-vec from-index index)
  (if (< index 0)
      'done
      (begin
	(vector-set! vec (+ from-index index) (vector-ref old-vec index))
	(vector-copy-helper vec old-vec from-index (- index 1)))))
			
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
