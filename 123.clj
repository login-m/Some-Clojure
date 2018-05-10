(def morph-R
  (fn [f L s]
    (if (empty? L)
      s
     (recur f (rest L) (cons (f (first L)) s)))))

(def morph
  (fn [f L]
    (morph-R f L '())))
