;
;  PERJURE3. The Level 3 Perjure interpreter.
;
;    Makar Baranov
;    02 Dec 16
;
;  Level 3 supports everything that Level 2 did, but also LETFN and recursive calls:
;  like what's in Clojure but not really.
;
;  It is fully based on Level 2 Perjure interpreter written by James Moen
; 


(def evaluate)  ;  To allow mutual recursion.

;  SCOPE. The global scope. SCOPEs are stacks (lists) of HASH-MAPs.

(def scope
 (list
  (assoc
   (hash-map)
   '+     +
   '-     -
   '*     *
   '/     /
   '=     =
   '<     <
   '>     >
   'true  true
   'false false)))

;  CLOSURE?. Test if OBJECT is a closure (but not a Clojure!) created by FN.

(def closure?
 (fn [object]
  (and
   (vector? object)
   (= (get object 0) 'closure))))

;  CLOSURE-BODY. Return the body of CLOSURE: a form.

(def closure-body
 (fn [closure]
  (get closure 2)))

;  CLOSURE-PARAMETERS. Return the parameter list of CLOSURE: a list of zero or
;  more distinct symbols.

(def closure-parameters
 (fn [closure]
  (get closure 1)))

;  CLOSURE-SCOPE. Return the defining scope of CLOSURE.

(def closure-scope
 (fn [closure]
  (deref (get closure 3))))

;  CONSTANT?. Test if FORM is a constant.

(def constant?
 (fn [form]
  (or
   (nil? form)
   (number? form)
   (string? form)
   (= form true)
   (= form false))))

;  FN-CALL?. Test if FORM is a call to the special form FN.

(def fn-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'fn))))

;  EVALUATE-CALL. Evaluate a call to a Perjure closure (but not a Clojure!).

(def evaluate-call
 (fn [closure arguments scope]
  (loop
   [parameters (closure-parameters closure)
    arguments  arguments
    map        (hash-map)]
   (if
    (empty? parameters)
    (evaluate
     (closure-body closure)
     (cons map
      (closure-scope closure)))
    (recur
     (rest parameters)
     (rest arguments)
     (assoc map
      (first parameters)
      (evaluate (first arguments) scope)))))))

;  EVALUATE-FN. Evaluate FORM, a call to the special form FN, in SCOPE.

(def evaluate-fn
 (fn [form scope]
  (vector 'closure
   (second form)
   (first (rest (rest form)))
   (atom scope))))

;  EVALUATE-IF. Evaluate FORM, a call to the special form IF, in SCOPE.

(def evaluate-if
 (fn [form scope]
  (if
   (evaluate (first (rest form)) scope)
   (evaluate (first (rest (rest form))) scope)
   (evaluate (first (rest (rest (rest form)))) scope))))

;  EVALUATE-LET. Evaluate FORM, a call to the special form LET, in SCOPE.

(def evaluate-let
 (fn [form scope]
   (loop [pairs (second form)
	  map   (hash-map)]
   (if (empty? pairs)
     (evaluate (first (rest (rest form))) (cons map scope))
   (recur (rest (rest pairs)) (assoc map (first pairs) (evaluate (second pairs) (cons map scope))))))))


; LETFN-REVERSE. Restores all the scopes changed by LETFN-APPLY

(def letfn-reverse
  (fn [scope scopes]
    (loop [traverse scope
           scopes (deref scopes)]
      (if (or (empty? scopes) (empty? traverse))
        true
      (do
        (reset! (last (second (first traverse))) (first scopes))
	(recur (rest (rest traverse)) (rest scopes)))))))

; LETFN-APPLY. Applies complete scope to each of the closure's scope to allow recursion

(def letfn-apply
  (fn [scope]
    (loop [traverse scope
           saved    (atom '())]
      (if (empty? traverse)
        saved
      (let [inner_scope (deref (last (second (first traverse))))]
        (do
          (reset! saved (concat (deref saved) (list inner_scope)))
          (reset! (last (second (first traverse))) (cons scope (deref (last (second (first traverse))))))
          (recur (rest traverse) saved)))))))

;  EVALUATE-LETFN. Evaluate FORM, a call to the special form LETFN, in SCOPE.

(def evaluate-letfn
 (fn [form scope]
   (loop [pairs (second form)
	  map   (hash-map)]
	  
   (if (empty? pairs)
     (let [saved_scopes (letfn-apply map)
	   result (evaluate (first (rest (rest form))) (cons map scope))]
       (if (letfn-reverse map saved_scopes) result nil))
       
   (recur (rest (rest pairs)) (assoc map (first pairs) (evaluate (second pairs) (cons map scope))))))))

    

;  EVALUATE-SYMBOL. Return the binding of SYMBOL in SCOPE.

(def evaluate-symbol
 (fn
  ([symbol]
   (evaluate-symbol symbol scope))

  ([symbol scope]
   (if
    (empty? scope)
    (throw (Exception. (str "Unbound symbol: " symbol)))
    (if
     (contains? (first scope) symbol)
     (get (first scope) symbol)
     (recur symbol (rest scope)))))))

;  IF-CALL?. Test if FORM is a call to the special form IF.

(def if-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'if))))

;  LET-CALL?. Test if FORM is a call to the special form LET.

(def let-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'let))))

; LETFN-CALL?. Test if FORM is a call to the special form LETFN

(def letfn-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'letfn))))

;  QUOTE-CALL?. Test if FORM is a call to the special form QUOTE.

(def quote-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'quote))))

;  EVALUATE. The level 2 Perjure evaluator.

(def evaluate
 (fn
  ([form]
   (evaluate form scope))

  ([form scope]
   (cond
    (constant? form)
    form

    (symbol? form)
    (evaluate-symbol form scope)

    (fn-call? form)
    (evaluate-fn form scope)

    (if-call? form)
    (evaluate-if form scope)

    (let-call? form)
    (evaluate-let form scope)

    (letfn-call? form)
    (evaluate-letfn form scope)

    (quote-call? form)
    (second form)

    true
    (let
     [function (evaluate (first form) scope)]
     (if
      (closure? function)
      (evaluate-call function (rest form) scope)
      (apply function
       (map
        (fn [argument]
         (evaluate argument scope))
        (rest form)))))))))



(evaluate '(letfn (factorial (fn (n) (if (= n 0) 1 (* n (factorial (- n 1)))))) (factorial 5)))


