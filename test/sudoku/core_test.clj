(ns sudoku.core-test
  (:use clojure.test
        sudoku.core))

(def puzzle1 [1 2 3 4
              3 4 1 2
              4 1 2 3
              2 3 4 1])

(def puzzle2 [2 1 3 4
              3 4 1 2
              4 1 2 3
              2 3 4 1])

(def puzzle3 [1 2 3 4
              3 4 1 2
              4 1 4 3
              2 3 2 1])

(def puzzle4 [1 2 3 4
              4 3 1 2
              3 1 2 4
              2 4 3 1])

(deftest solving-test-1
  (testing ""
    (is (is-solved? puzzle1))))

(deftest solving-test-2
  (testing ""
    (is (not (is-solved? puzzle2)))))

(deftest solving-test-3
  (testing ""
    (is (not (is-solved? puzzle3)))))

(deftest solving-test-4
  (testing ""
    (is (not (is-solved? puzzle4)))))