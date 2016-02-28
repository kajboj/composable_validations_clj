(ns composable-validations-clj.core)

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
      [false (merge-errors e {p [message]})])))

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

(defn is-of-type
  "validator ensuring type of object"
  [klass message]
  (validate #(instance? klass %1) message))

(defn string
  "validator ensuring that validated object is a string"
  [& {:keys [msg] :or {msg :string}}]
  (is-of-type String msg))

(defn just-object
  "validator ensuring that validated object is a hash map (JSON object)"
  [& {:keys [msg] :or {msg :just-object}}]
  (is-of-type clojure.lang.PersistentArrayMap msg))
