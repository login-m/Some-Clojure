
(def get-args
  (fn [L e]
   (if (empty? L)
       '()
        (concat (list (concat '(=) (list (first L)) (list e))) (list (first (rest L))) (get-args (rest (rest L)) e)))))

(defmacro which [S & args]
    (concat
           '(let) (list (vector 'elem S))
            (list (concat  '(cond)
	    (get-args args 'elem) '(:else nil)))))

;*********************

(def expand
  (fn [e]
    `(if ~e 1 0)))

(defmacro most [& args]
    `(if (> (+ ~@(map expand args)) ~(/ (count args) 2))
	    true false))
	    
;*********************

(def nest
  (fn [string N]
    (if (= N 0)
      'x
      (cons string (list (nest string (- N 1)))))))

(defmacro comps [fi k]
  (if (= k 0) '(fn [x] x)
  (if (= k 1) fi
  (concat '(let) (list (vector 'g# fi)) (list (concat '(fn [x]) (list (nest 'g# k))))))))
       
;*********************

(defmacro qrat [s a b c]
  (if (or (= a 0) (< (* b b) (* 4 (* a c))))
     nil
     (concat '(/)
      (list (concat (list s) (list (concat '(-) (list b)))
      (list (concat '(Math/sqrt) (list (concat '(-) (list (list '* b b)) (list (concat (list '* 4) (list (list '* a c))))))))))														
      (list (list '* 2 a)))))
   


