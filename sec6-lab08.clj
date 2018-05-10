(declare lambda?)
(declare lambda-call?)
(declare lambda-fn?)
(declare lambda-symbol?)


;********************************************************************

(def contains-helper
  (fn [S e]
    (if (empty? S)
      'false
    (if
       (= (first S) e) 'true
       (recur (rest S) e)))))


(def unique?
  (fn [S]
    (if (empty? S)
      'true
    (if (contains-helper (rest S) (first S))
      'false
      (recur (rest S))))))     

(def mergevec-helper
  (fn [new old]
    (if (empty? old) new
      (mergevec-helper (conj new (first old)) (rest old)))))


(def mergevec
  (fn [new old]
     (into [] (distinct (mergevec-helper new old)))))
      

(def lambda-parameters?
  (fn [P]
    (and (vector? P) (unique? P))))


(def lambda-fn?
  (fn [L S]
    (if (and (= 'fn (first L)) (lambda-parameters? S)) 'true 'false)))

;********************************************************************

(def lambda-symbol?
  (fn [N S]
    (contains-helper S N)))


(def lambda-call?
  (fn [L S]
    (if (empty? L) 'true
     (if (symbol? (first L)) (and (lambda-symbol? (first L) S) (lambda-call? (rest L) S))
     (if (= (first (first L)) 'fn) (and (lambda? (first L) (if (unique? (second (first L))) (mergevec S (second (first L))) '(nil))) (lambda-call? (rest L) S))
     (and (lambda-call? (first L) S) (lambda-call? (rest L) S)))))))
	

;********************************************************************

(defn lambda?
  ([L]
    (if (seq? L)
     (if (lambda-fn? L (second L))
       (lambda? L (second L)) 'false) 'false))
  ([L S]
    (if (lambda-fn? L S)
      (let [exp (last L)]
        (if (symbol? exp) (lambda-symbol? exp S)
        (if (= (first exp) 'fn) (lambda? exp (mergevec S (second exp)))
	(lambda-call? exp S)))) 'false)))
	
	



