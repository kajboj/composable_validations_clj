(ns composable-validations-clj.core-test
  (:require [clojure.test :refer :all]
            [composable-validations-clj.core :refer :all]))

(def always-true (fn [o] true))
(def always-false (fn [o] false))
(def always-success (validate always-true "error"))
(def always-failure (validate always-false "error"))

(deftest composable-validations

  (testing "validate function success"
    (is (=
          ((validate always-true "error") {} {} [])
          [true {}])))

  (testing "validate function failure"
    (is (=
          ((validate always-false "error") {} {} ["element"])
          [false {["element"] ["error"]}])))

  (testing "fail-fast combinator with no validators"
    (is (=
          ((fail-fast) {} {} ["element"])
          [true {}])))

  (testing "fail-fast combinator success"
    (is (=
          ((fail-fast always-success) {} {} ["element"])
          [true {}]))
    (is (=
          ((fail-fast always-success always-success) {} {} ["element"])
          [true {}])))

  (testing "fail-fast combinator failure"
    (is (=
          ((fail-fast always-failure) {} {} ["element"])
          [false {["element"] ["error"]}]))
    (is (=
          ((fail-fast always-failure always-success) {} {} ["element"])
          [false {["element"] ["error"]}]))
    (is (=
          ((fail-fast always-success always-failure) {} {} ["element"])
          [false {["element"] ["error"]}]))))
