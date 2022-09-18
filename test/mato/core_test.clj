(ns mato.core-test
  (:require [clojure.test :refer :all]
            [mato.core :refer :all]
            ))

(deftest test-create-coord
  (is (map? (create-coord 1 1)))
  (is (every? (create-coord 1 1) [:x :y])))

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

(deftest test-move-worm
  (testing "movement without grow"
    (is (= [{:x 2 :y 2}] (move-worm [(create-coord 1 2)] right false)))
    (is (= [{:x 2 :y 2}] (move-worm [(create-coord 2 1)] down false))))
  (testing "movement with grow"
    (is (= [{:x 2 :y 2} {:x 1 :y 2}] (move-worm [(create-coord 1 2)] right true)))
    (is (= [{:x 2 :y 2} {:x 2 :y 1}] (move-worm [(create-coord 2 1)] down true)))))

(deftest test-collision
  (testing "no collision"
    (are [expected result]
      (= expected result)
      false (collision? [(create-coord 1 0) (create-coord 1 1)])
      false (collision? [(create-coord 1 1) (create-coord 2 1)])
      false (collision? [(create-coord 0 1) (create-coord 1 1)])
      false (collision? [(create-coord (dec scene-width) 1)])
      false (collision? [(create-coord 1 (dec scene-height))])))
  (testing "collision"
    (are [expected result]
      (= expected result)
      true (collision? [(create-coord 0 -1)])
      true (collision? [(create-coord -1 0)])
      true (collision? [(create-coord scene-width 1)])
      true (collision? [(create-coord 1 scene-height)]))))

(deftest test-has-coord-in-it
  (is (= true (has-coords-in-it? 1 [2 1])))
  (is (nil? (has-coords-in-it? 1 [2 3]))))

(deftest test-will-eat
  (testing "eating"
    (are [expected result]
      (= expected result)
      true (will-eat? [(create-coord 1 1)] [(create-coord 0 1)] left)
      true (will-eat? [(create-coord 1 1)] [(create-coord 2 1)] right)))

  (testing "not eating"
    (are [expected result]
      (= expected result)
      nil (will-eat? [(create-coord 1 1)] [(create-coord 0 1)] right)
      nil (will-eat? [(create-coord 1 1)] [(create-coord 0 1)] up))))

(deftest test-remove-item-if
  (is (= [1 3] (remove-item-if true 2 [1 2 3])))
  (is (= [1 2 3] (remove-item-if false 2 [1 2 3]))))

(deftest test-engine-step
  (testing "movements"
    (let [worm [(create-coord 1 1)] 
          goodies [(create-coord 0 0)]]
      (let [[resulted-worm resulted-goodies] (engine-step right worm goodies)]
        (is (= [(create-coord 2 1)] resulted-worm))
        (is (= goodies resulted-goodies)))))
  (testing "worm growth"
    (let [worm [(create-coord 1 1)] 
          goodies [(create-coord 2 1)]]
      (let [[resulted-worm resulted-goodies] (engine-step right worm goodies)]
        (is (= [(create-coord 2 1) (create-coord 1 1)] (into [] resulted-worm)))
        (is (empty? resulted-goodies))))))

(comment
  (run-all-tests))

