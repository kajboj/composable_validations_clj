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

(defmacro def-validate-type-and-run-all
  [name description type-validator]
  `(defn ~name
     ~description
     [& ~'validators]
     (fail-fast
       (~type-validator)
       (apply run-all ~'validators))))

(def-type-validator string "a string" String)

(def-type-validator just-object
  "an object"
  clojure.lang.PersistentArrayMap)

(def-validate-type-and-run-all
  object
  "an object passing validations"
  just-object)

(def-type-validator just-array
  "an array"
  clojure.lang.PersistentVector)

(def-validate-type-and-run-all
  array
  "an array passing validations"
  just-array)

(defn presence-of-key
  "validates that hash contains given key"
  [key & {:keys [msg] :or {msg [:presence-of-key key]}}]
  (validate #(contains? %1 key) msg))

(defn has-key
  "required key passing validations"
  [key & validators]
  (fail-fast
    (presence-of-key key)
    (fn [o e p]
      ((apply run-all validators) (o key) e (conj p key)))))
