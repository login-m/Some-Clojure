;helper matching function
(def matching
  (fn [table pattern subject]
  
   ;case 1
   (if (keyword? pattern)                ;if pattern is a keyword
     (if (= (get table pattern) nil)     ;if the key is not in the table, put the value and the key in there
        (assoc table pattern subject)   
     (if (= (get table pattern) subject) ;if the value of the key equals to subject, return table
        table nil))                      ;otherwise the key is used, return nil

   ;case 2 & 3
   (if
     (and
        (seq? pattern)                   ;check if both are non empty seqs
	(seq? subject)
        (not (empty? pattern))
        (not (empty? subject)))
     (matching (matching table (first pattern) (first subject)) (rest pattern) (rest subject)) ;if yes, update the table and then call the function with the rest.
     (if (= pattern subject) table nil)))))                                                    ;if no, base case -> check if two things are equal
                                                                                               ;return table if it is, nil otherwise.

;main match function
(def match
  (fn [pattern subject]
      (matching (hash-map) pattern subject)))