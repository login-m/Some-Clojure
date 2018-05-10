;*************************
; Small Helper Functions *
;*************************

(def op
  (fn [s]
    (first s)))

(def left
  (fn [s]
    (second s)))

(def right
  (fn [s]
    (first (rest (rest s)))))


(def if-then
  (fn [s]
    (first (rest (rest s)))))
    

(def if-else
  (fn [s]
    (first (rest (rest (rest s))))))


;************************
; Main Helper Functions *
;************************

; IFIFY

(def ifify
  (fn [P]
    (if (seq? P)
      (let [operator (op P) if1 '(if) true1 '(true) false1 '(false)]
        (if (= operator 'not)
          (concat if1 (list (ifify (left P))) false1 true1)
	(if (= operator 'and)
	  (concat if1 (list (ifify (left P))) (list (ifify (right P))) false1)
	(if (= operator 'or)
	  (concat if1 (list(ifify (left P))) true1 (list (ifify (right P))))
	(if (= operator 'imply)
	  (concat if1 (list (ifify (left P))) (list (ifify (right P))) true1)
	(if (= operator 'equiv)
	  (concat if1 (list (ifify (left P))) (list (ifify (right P))) (list (concat if1 (list (ifify (right P))) false1 true1)))
	))))))
     P)))



;*********************

; NORMALIZE

(def normalize
  (fn [C]
    (if (not (seq? C)) C
    (if (and (= 'if (first C)) (seq? (second C)) (= 'if (first (second C))))
      (normalize
        (concat
	  '(if)
          (list (second (second C)))
      	  (list (normalize (concat '(if) (list (if-then (second C))) (list (if-then C)) (list (if-else C)))))
	  (list (normalize (concat '(if) (list (if-else (second C))) (list (if-then C)) (list (if-else C)))))))

    (if (not (seq? (second C)))
    	    (concat '(if)
            (list (second C))
	    (if (seq? (if-then C)) (list (normalize (if-then C))) (list (if-then C)))
            (if (seq? (if-else C)) (list (normalize (if-else C))) (list (if-else C)))) '())))))
	    

;*********************

; SUBSTITUTE

(def contains-helper
  (fn [s e]
    (if
      (empty? s) false
      (if
        (= (first s) e) true
	(recur (rest s) e)))))
	

;(def substitute
 ; (fn [C V B]
    ;(if (empty? C)
     ; '()
    ;(concat
     ; (if (= (first C) V) (list B) (list (first C))) (substitute (rest C) V B))))))

(def substitute
  (fn [C V B]
    (if (seq? C)
      (if (empty? C) '()
	             (list 'if (substitute (second C) V B) (substitute (if-then C) V B) (substitute (if-else C) V B)))
    (if (= C V) B C))))


;*********************

; SIMPLIFY

(def simplify
  (fn [P]
    (if (seq? P)
      (if (empty? P) '()
      (if (= 'true (second P)) (simplify (if-then P))
      (if (= 'false (second P)) (simplify (if-else P))

      (if (and (= 'true (if-then P)) (= 'false (if-else P))) (second P)
      (if (= (if-then P) (if-else P)) (simplify (if-then P))
      (if (or (seq? (if-then P)) (seq? (if-else P)))
        (let [expression
		        (concat '(if) (list (second P)) (list (simplify (substitute (if-then P) (second P) 'true)))
		                                        (list (simplify (substitute (if-else P) (second P) 'false))))]
          (if (= expression P) P (simplify expression)))
       P))))))
      P )))


;**********************

; TAUTOLOGY?

(def tautology?
  (fn [P]
     (if (= 'true (simplify (normalize (ifify P))))
       'true 'false)))

