;e - current element
;c - set
;n - number of elements
;k - k elements from a set of n elements
;etc - cont functions

(declare thru)
(declare choose)

;kind of a for loop
(def thru
  (fn [etc start end]
    (loop [index start]
      (if (< index end)
        (do
	  (etc index)
	  (recur (+ index 1)))))))

;helper function
(def choosing
  (fn [etc c n k e]
    (if (and (>= n k) (not (= k 0)))
      (thru
        (fn [element]
	 (if (not (= (last c) element))
	   (choosing etc (concat c (list element)) n (- k 1) element)))
       e n)
       (etc c))))

     
;main function
(def choose
  (fn [etc n k]
    (choosing etc '() n k 0)))
