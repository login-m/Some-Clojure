(declare unpreceded)
(declare unpreceded?)
(declare precedes?)
(declare delete)
(declare satisfy)

(def partial-ordering 
  (atom (hash-map 
    'A  #{'C} 
    'B  #{'H} 
    'C  #{'G} 
    'G  #{'D 'E} 
    'D  #{'F} 
    'E  #{'H} 
    'H  #{'F} 
    'I  #{'B 'E})))


; SATISFY and SATISFY-HELPER

(def satisfy-helper
  (fn [precedes? objects result]
    (if (empty? (deref partial-ordering)) (concat result objects)
    (let [x (unpreceded unpreceded? precedes? objects)]
      (if (= x nil) 'nil
        (do
          (reset! partial-ordering (dissoc (deref partial-ordering) x))
          (recur precedes? (delete objects x) (concat result (list x)))))))))


(def satisfy
  (fn [precedes? objects]
    (satisfy-helper precedes? objects '())))







; UNPRECEDED

(def unpreceded
  (fn [etc precedes? objects]
    (loop [val (first objects)
           L objects
           traverse (rest objects)]
    (if (= val nil) nil
    (if (etc precedes? val L) val
	(recur (first traverse) L (rest traverse) ))))))







; UNPRECEDED?

(def some-helper
  (fn [P e S]
    (if (empty? S)
      false
    (if (P e (first S))
      true
    (recur P e (rest S))))))

(def unpreceded?
  (fn [precedes? object other-objects]
    (not (some-helper precedes? object other-objects))))







; PRECEDES?

(def contains-helper
  (fn [S e]
    (if (empty? S)
      'false
    (if
       (= (first S) e) 'true
       (recur (rest S) e)))))

(def precedes?
  (fn [left right]
    (contains? (get (deref partial-ordering) right) left)))





; DELETE

(def delete
  (fn [objects object]
    (remove (fn [x] (= x object)) objects)))



