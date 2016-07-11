(ns composable-validations-clj.core)

(defprotocol ErrorCollection
  "Collection of validation errors"
  (add-error [errors path message object] "adding new error to the collection"))

(extend-type
  clojure.lang.PersistentArrayMap
  ErrorCollection
  (add-error [this path message object]
    (merge-with concat this {path [message]})))

(defn validate
  "helper function to build validators from predicates"
  [pred message]
  (fn [o e p]
    (if (pred o)
      [true e]
      [false (add-error e p message o)])))

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
    (reduce
      (fn [[acc-result acc-errors] validator]
        (let [[result errors] (validator o acc-errors p)]
          [(and acc-result result) errors]))
      [true e]
      validators)))

(defn is-of-type
  "validator ensuring type of object"
  [klass message]
  (validate #(instance? klass %1) message))

(defmacro def-type-validator
  [name description klass]
  `(defn ~name
     ~(clojure.string/join " "
       ["validator ensuring that validated object is" description])
     [& {:keys [~'msg] :or {~'msg ~(keyword name)}}]
     (is-of-type ~klass ~'msg)))

(def-type-validator string "a string" String)

(def-type-validator just-object
  "an object"
  clojure.lang.PersistentArrayMap)

(defn object
  "an object passing validations"
  [& validators]
  (fail-fast
    (just-object)
    (apply run-all validators)))
