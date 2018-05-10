(declare euler)
(declare eulerhelp)

(def eulerhelp
  (fn [x term count]
     (list term
           (fn [] (eulerhelp x (* term (/ x count)) (+ count 1))))))


(def euler
  (fn [x]
    (eulerhelp x 1.0 1.0)))


(def nt
  (fn [terms]
    (first terms)))

(def rt
  (fn [terms]
    ((first (rest terms)))))


(def sum-helper
  (fn [e epsilon sum]
    (if (< (nt e) epsilon)
      sum
    (recur (rt e) epsilon (+ sum (nt e))))))


(def sum
  (fn [e epsilon]
    (sum-helper e epsilon 0.0)))
      
