(ns composable-validations-clj.core-test
  (:require [clojure.test :refer :all]
            [composable-validations-clj.core :refer :all]))

(def always-true (fn [o] true))
(def always-false (fn [o] false))
(def always-success (validate always-true "error"))
(def always-failure (validate always-false "error"))
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

  (testing "validate function success"
    (is-valid (validate always-true "error") standard-args))

  (testing "validate function failure"
    (is-invalid (validate always-false "error")
                standard-args
                {["element"] ["error"]}))

  (testing "fail-fast combinator with no validators"
    (is-valid (fail-fast) standard-args))

  (testing "fail-fast combinator success"
    (is-valid (fail-fast always-success) standard-args)
    (is-valid (fail-fast always-success always-success) standard-args))

  (testing "fail-fast combinator failure"
    (is-invalid (fail-fast always-failure)
                standard-args
                {["element"] ["error"]})
    (is-invalid (fail-fast always-failure always-success)
                standard-args
                {["element"] ["error"]})
    (is-invalid (fail-fast always-success always-failure)
                standard-args
                {["element"] ["error"]})))
