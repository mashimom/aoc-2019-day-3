(ns day-3.core-test
  (:require [clojure.test :refer :all]
            [day-3.core :refer :all]))

(deftest text-to-azimuth
  (testing "Check if converts correctly"
    (is (= (to-azimuth "R8") {:direction :right, :value 8}))
    (is (= (to-azimuth "U5") {:direction :up, :value 5}))
    (is (= (to-azimuth "L5") {:direction :left, :value 5}))
    (is (= (to-azimuth "D3") {:direction :down, :value 3}))
    (is (= (to-azimuth "L12") {:direction :left, :value 12}))))

(deftest walk-works
  (testing "walk works"
    (is (= (walk [0 0] {:direction :right, :value 8}) [8 0]))
    (is (= (walk [-3 -4] {:direction :up, :value 1}) [-3 -3]))
    (is (= (walk [158 -30] {:direction :left, :value 12}) [146 -30]))))

(deftest to-coordinates-works
  (testing "to-coordinates works"
    (let [azimuths [{:direction :right, :value 8}
                    {:direction :up, :value 5}
                    {:direction :left, :value 5}
                    {:direction :down, :value 3}]]
      (is (= (to-coordinates azimuths) [[0 0] [8 0] [8 5] [3 5] [3 2]]))
      (is (= (to-coordinates [10 10] azimuths) [[10 10] [18 10] [18 15] [13 15] [13 12]])))))

(deftest to-wire-works
  (testing "to-wire works"
    (is (= (to-wire ["R8" "U5" "L5" "D3"]) [[[0 0] [8 0]]
                                            [[8 0] [8 5]]
                                            [[8 5] [3 5]]
                                            [[3 5] [3 2]]]))
    (is (= (map to-wire [["R8" "U5" "L5" "D3"]
                         ["U7" "R6" "D4" "L4"]])
           [[[[0 0] [8 0]] [[8 0] [8 5]] [[8 5] [3 5]] [[3 5] [3 2]]]
            [[[0 0] [0 7]] [[0 7] [6 7]] [[6 7] [6 3]] [[6 3] [2 3]]]]))))


(deftest intersection-works
  (testing "intersection works"
    (is (= (intersection [[1 1] [4 1]] [[2 -2] [2 2]]) [2 1]))
    (is (nil? (intersection [[3 1] [4 1]] [[2 -2] [2 2]]))))
  (testing "intersection-line works"
    (is (= (intersection-line [[1 1] [4 1]] [[2 -2] [2 2]]) [2 1]))
    (is (nil? (intersection-line [[3 1] [4 1]] [[2 -2] [2 2]])))))

(deftest manhattan-distance-works
  (testing "manhattan-distance works"
    (is (= (manhattan-distance [2 2]) 4))
    (is (= (manhattan-distance [-2 -2]) 4))))
