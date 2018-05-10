;***************************************
; Unique Function
;***************************************

;Hepler function to check if the rest of the seq contains passed values
(def contains-helper
  (fn [s e]
    (if
      (empty? s) false
      (if
        (= (first s) e) true
	(recur (rest s) e)))))
	
;Main function
(def unique
  (fn [F s]
    (if (not (empty? s))
      (if (contains-helper (rest s) (first s))
        (recur F (rest s))
	(do
	  (F (first s))
	  (recur F (rest s)))))))



;***************************************
; Nest Function
;***************************************


(def nest
  (fn [F N]
    (fn [x]
      (loop [num x count N]
        (if (= count 0)
	  num
	(recur (inc num) (- count 1)))))))
 

;***************************************
; Cartesian Function
;***************************************

;Helper function  to map each value of u to all the elements of v
(def mappy
  (fn [F current elements]
   (if
     (empty? elements)
    '()
    (concat
      (list(F current (first elements)))
      (mappy F current (rest elements))))))


;Main function
(def cartesian
  (fn [F u v]
    (if (empty? u)
      '()
    (concat (mappy F (first u) v) (cartesian F (rest u) v)))))
