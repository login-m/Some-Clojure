

(def error
 (fn [message]
  (throw (Exception. message))))



(def Queue
 (fn []
  (letfn

   [(make-queue [top]
     (fn [method & arguments]
      (letfn

       [(empty? []
         (= top '()))

        (enqueue [e]
         (make-queue (concat top (list e))))

        (dequeue []
         (if
          (empty?)
          (error "Queue is empty.")
          (make-queue (rest top))))

        (front []
         (if
          (empty?)
          (error "Queue is empty.")
          (first top)))]


       (cond
        (= method :empty?)
        (empty?)

        (= method :enqueue)
        (enqueue (first arguments))

        (= method :dequeue)
        (dequeue)

        (= method :front)
        (front)

        true
        (error "No such method.")))))]

   (make-queue '()))))


