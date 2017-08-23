(ns game-of-life.core-test
  (:require [clojure.test :refer :all]
            [game-of-life.core :refer :all]))

(def mock-width 4)
(def mock-height 4)
(def mock-board
  {:width mock-width
   :height mock-height
   :cells (into [] (map
                    (partial assoc {:alive false} :id)
                    (range 0 (* mock-width mock-height))))})

(deftest find-above
  (is (=
       nil
       (find-above-neighbor mock-board ((comp first :cells) mock-board))))
  (is (=
       (first (:cells mock-board))
       (find-above-neighbor mock-board (nth (:cells mock-board) 4)))))

(deftest life-index-fill
  (is (every? #(= 0 %) (map :life-index (:cells (fill-board-life-index mock-board))))))

(deftest neighbors
  (is (= 3 (count (find-neighbors mock-board (first (:cells mock-board)))))))

(deftest game-tick
  (is (= ((comp tick tick) mock-board) (tick mock-board))))
