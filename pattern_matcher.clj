;; follow up with the pattern matcher example on simply scheme book
;; but implemented in clojure for fun

;; but-first, like in simply.scm

(defn bf [sent] (rest sent))

(defn sent-equal? [sent1 sent2]
  (cond 
    (empty? sent1) (empty? sent2)
    (empty? sent2) false
    (= (first sent1) (first sent2)) (sent-equal? (bf sent1) (bf sent2))
    :else false))
  
	
