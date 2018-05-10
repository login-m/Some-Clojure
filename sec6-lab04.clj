(declare operator)
(declare left-arg)
(declare right-arg)
(declare stackify)


;Helper function
(def stack
  (fn [form]
    ;hash table to map functions to their stack representation
    (let [map (hash-map '- '((sub)) '+ '((add)) '* '((mul)) '/ '((div)) 'neg '((neg)) 'push '(push))]
       
      ;if the form is a number or a symbol
      (if (not (seq? form))
        (concat (list (concat (get map 'push) (list form))))

      ;if the form has two arguments: operator neg and a number/symbol
      (if (and (= '- (operator form)) (= (right-arg form) nil))
        (let [x (if (seq?  (left-arg form))  (stack (left-arg form))  (list(left-arg form)))]
	  (concat
	    (if (not (empty? (rest x))) x (list (concat (get map 'push) x)))
	    (get map 'neg)))

      ;if the form has three arguments
      (if (= (count form) 3)
        ;if x/y seqs -> recur, otherwise return a listed symbol
        (let [x (if (seq?  (left-arg form))  (stack (left-arg form))  (list(left-arg form)))
	      y (if (seq? (right-arg form)) (stack (right-arg form)) (list(right-arg form))) ]
	      
	;concatenate the results with push if needed and the operator 
	(concat
	   (if (not (empty? (rest x))) x (list (concat (get map 'push) x)))
	   (if (not (empty? (rest y))) y (list (concat (get map 'push) y)))
	   (get map (operator form))))))))))
	 
 

;Three Helper Functions
;1 - returns operator
;2 - returns left argument
;3 - returns right argument

(def operator
  (fn [list]
    (first list)))
    
(def left-arg
  (fn [list]
    (second list)))
    
(def right-arg
  (fn [list]
    (first (rest (rest list)))))


;Main function
(def stackify
  (fn [form]
    (concat (stack form) '((pop)))))

