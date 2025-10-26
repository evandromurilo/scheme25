;; follow up with the pattern matcher example on simply scheme book
;; but implemented in clojure for fun
;;
;; ? At most one word
;; ! Exactly one word
;; & At least one word
;; * Any number of words

;; but-first, like in simply.scm

(defn bf [sent] (rest sent))

(defn sent-equal? [sent1 sent2]
  (cond 
    (empty? sent1) (empty? sent2)
    (empty? sent2) false
    (= (first sent1) (first sent2)) (sent-equal? (bf sent1) (bf sent2))
    :else false))
  
(defn match? [pattern sent]
  (cond
    (empty? pattern) (empty? sent)
    (= (first pattern) '?) (if (empty? sent)
                             (match? (bf pattern) '()) ;; empty sentence, will match if empty patterna after ?
                             (or (match? (bf pattern) (bf sent)) ;; otherwise will match the first word for ?, or just skip the ?
                                 (match? (bf pattern) sent)))
    (empty? sent) false
    (= (first pattern) '!) (match? (bf pattern) (bf sent))
    (= (first pattern) (first sent)) (match? (bf pattern) (bf sent))
    :else false))
