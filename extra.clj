
; EXTRA CREDIT

(def expand
  (fn [args]
    (if (empty? (rest args)) (list (first args))
	(concat (list (list 'not (first args))) (expand (rest args))))))

(defmacro imply [a & args]
  (if (empty? args)
	a
  (concat '(or) (expand (concat (list a) args)))))
