(ns composable-validations-clj.core-test
  (:require [clojure.test :refer :all]
            [composable-validations-clj.core :refer :all]))

(def always-true (fn [o] true))
(def always-false (fn [o] false))
(def success (validate always-true "error"))
(defn failure
  ([message] (validate always-false message))
  ([] (failure "error")))
(def standard-args '({} {} ["element"]))

(defn is-valid
  "asserts that validator succeeds when applied to standard args"
  [validator args]
  (is (=
       (apply validator args)
       [true {}])))

(defn is-invalid
  "asserts that validator fails with errors when applied to standard args"
  [validator args errors]
  (is (=
       (apply validator args)
       [false errors])))


(deftest composable-validations

  (testing "validate"
    (is-valid (validate always-true "error") standard-args)
    (is-invalid (validate always-false "error")
                standard-args
                {["element"] ["error"]})
    (is-invalid (validate always-false "error2")
                '({} {["element"] ["error1"]} ["element"])
                {["element"] ["error1" "error2"]}))

  (testing "fail-fast"
    (is-valid (fail-fast) standard-args)
    (is-valid (fail-fast success) standard-args)
    (is-valid (fail-fast success success) standard-args))
    (is-invalid (fail-fast (failure))
                standard-args
                {["element"] ["error"]})
    (is-invalid (fail-fast (failure) success)
                standard-args
                {["element"] ["error"]})
    (is-invalid (fail-fast success (failure))
                standard-args
                {["element"] ["error"]})

  (testing "run-all"
    (is-valid (run-all) standard-args)
    (is-valid (run-all success) standard-args)
    (is-valid (run-all success success) standard-args))
    (is-invalid (run-all (failure))
                standard-args
                {["element"] ["error"]})
    (is-invalid (run-all (failure) success)
                standard-args
                {["element"] ["error"]})
    (is-invalid (run-all success (failure))
                standard-args
                {["element"] ["error"]})
    (is-invalid (run-all (failure "error1") (failure "error2"))
                standard-args
                {["element"] ["error1" "error2"]})
    (is-invalid (run-all (failure "error1") (failure "error2"))
                '({} {:a :b} ["element"])
                {:a :b ["element"] ["error1" "error2"]})

  (testing "is-of-type"
    (is-valid (is-of-type String "error") '("hello" {} ["element"]))
    (is-invalid (is-of-type String "error")
                '(1 {} ["element"])
                {["element"] ["error"]}))

  (testing "string"
    (is-valid (string) '("hello" {} ["element"]))
    (is-invalid (string)
                '(1 {} ["element"])
                {["element"] [:string]})))
