(ns mato.core-test
  (:require [clojure.test :refer :all]
            [mato.core :refer :all]))

(deftest test-change-coord
  (testing "change-coord creates a hash-map, expecting two maps containing :x and :y"
    (let [x1 1
          x2 10
          y1 2
          y2 20
          expected-x (+ x1 x2)
          expected-y (+ y1 y2)]
      (is (= (hash-map :x expected-x :y expected-y) (mato.core/change-coord {:x x1 :y y1} {:x x2 :y y2})))))
  (testing "movement :x and :y are optional and zero by default"
    (let [x1 1
          y1 2
          expected-x x1
          expected-y y1
          empty-map {}]
      (is (= (hash-map :x expected-x :y expected-y) (mato.core/change-coord {:x x1 :y y1} empty-map))))))

(comment
  (run-all-tests))
