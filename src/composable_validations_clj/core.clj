(ns composable-validations-clj.core)

;; TODO: make sure that it preserves errors already existing at the path
(defn add-error
  "adds error at the path"
  [e p message]
  (assoc e p [message]))

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
