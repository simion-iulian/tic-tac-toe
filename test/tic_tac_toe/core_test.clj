(ns tic-tac-toe.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-toe.core :refer :all]))

(deftest neighbors-test
  (testing "that neigbors gives the expected values"
    (is (= (neighbors [0 0]) [[1 0] [0 1] [1 1]]))
    (is (= (neighbors [1 1]) [[0 1] [2 1] [1 0] [1 2] [2 2] [0 0] [2 0] [0 2]]))
    (is (= (neighbors [2 2]) [[1 2] [2 1] [1 1]]))))
