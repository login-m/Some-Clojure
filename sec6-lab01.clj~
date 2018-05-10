(def epsilon 0.000001) 

;body for exponential function
(def exp
  (fn [x F S T]
     (if (<= T epsilon)
        S
     (recur x (+ F 1.0) (+ S T) (* T (/ x F))))))
     
;helper exponential function
(def exponential
   (fn [x]
      (exp x 1.0 0.0 1.0)))

;helper abs function
(def abs
  (fn [x]
    (if (> x 0) x (* -1 x))))
    
;body for square root function
(def sqrt
  (fn [G H x]
    (if (< (abs (- G H))  epsilon)
      G
    (recur  (/ (+ G H) 2.0) (/ x (/ (+ G H) 2.0)) x))))


;helper square root function
(def square-root
  (fn [x]
    (sqrt x 1.0 x)))
