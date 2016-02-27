(ns composable-validations-clj.core)

;; TODO: make sure that it preserves errors already existing at the path
(defn add-error
  "adds error at the path"
  [e p message]
  (assoc e p [message]))

(defn merge-errors
  "merges two collections of errors ensuring no loss of error messages"
  [e1 e2]
  (merge-with concat e1 e2))

(defn validate
  "helper function to build validators from predicates"
  [pred message]
  (fn [o e p]
    (if (pred o)
      [true e]
      [false (add-error e p message)])))

(defn fail-fast
  "combinator returning errors of first failing validator"
  [& validators]
  (fn [o e p]
    (loop [vs validators]
      (if (empty? vs)
        [true {}]
        (let [first-result ((first vs) o e p)]
          (if (= (first first-result) true)
            (recur (rest vs))
            first-result))))))

(defn run-all
  "combinator running all validators and collecting all of their errors"
  [& validators]
  (fn [o e p]
    (reduce (fn [[acc-result acc-errors] [result errors]]
              [(and acc-result result) (merge-errors acc-errors errors)])
            [true e]
            (map #(%1 o {} p) validators))))
