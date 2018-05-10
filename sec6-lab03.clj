(declare mergesort)
(declare merging)
(declare splitting)


;//splitting function
(def splitting
  (fn [less? unsorted left right]
           
    ;//if list has 2 or more elements
    (if (>= (count unsorted) 2)                         
      (splitting less? (rest (rest unsorted)) (cons (first unsorted) left) (cons (second unsorted) right))

    ;//if list has 1 element
    (if (not (= (first unsorted) nil))
      (merging less? (mergesort less? (cons (first unsorted) left)) (mergesort less? right))

    ;//if list is empty
    (if (empty? unsorted)
      (merging less? (mergesort less? left) (mergesort less? right)))))))


;//merging function
(def merging
  (fn [less? left right]
    (if (empty? left) right                                       ;//check empty conditions
    (if (empty? right) left
    (if (less? (first left) (first right))                        ;//check the first values and merge with corresponding one
    	(cons (first left) (merging less? (rest left) right))
	(cons (first right) (merging less? left (rest right))))))))


;//mergesort function
(def mergesort
  (fn [less? unsorted]
    (if (or (empty? unsorted) (empty? (rest unsorted))) ;//check if list empty or has a single element, true -> return
      unsorted					 	;//otherwise call splitting using the list and empty left/right seqs
       (splitting less? unsorted '() '() ))))

