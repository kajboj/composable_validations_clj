(ns composable-validations-clj.integration-test
  (:require [clojure.test :refer :all]
            [composable-validations-clj.core :refer :all]))

(deftest composable-validations
  (testing "valid payload"
    (is (=
         ((object
           (has-key :a (string))
           (has-key :b (string)))
          {:a "hello" :b "world"} {} [])
         [true {}])))

  (testing "invalid payload"
    (is (=
         ((object
           (has-key :a (string))
           (has-key :b (object
                         (has-key :c string))))
          {:a "hello" :b {:c 1}} {} [])
         [true {}])))
