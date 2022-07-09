(ns mato.core-test
  (:require [clojure.test :refer :all]
            [mato.core :refer :all]))

(deftest test-change-coord
  (testing "change-coord creates a hash-map, expecting two maps containing :x and :y"
    (let [x1 1
          x2 10
          y1 2
          y2 20
          x (+ x1 x2)
          y (+ y1 y2)]
      (is (= (hash-map :x x :y y) (mato.core/change-coord {:x x1 :y y1} {:x x2 :y y2}))))))

(comment
  (run-all-tests))
