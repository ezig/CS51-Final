(ns npuzzles.puzzle-test
  (:require [clojure.test :refer :all]
            [npuzzles.puzzle :as puzzle :refer :all]))

(deftest swap-test
	(testing "Swap")
		(def swap #'puzzle/swap)
		(let [v [1 2 3 4 5]]
			(is (= (swap v 0 1) [2 1 3 4 5]))
			(is (= (swap v 1 0) [2 1 3 4 5]))
			(is (= (swap v 0 4) [5 2 3 4 1]))))

(deftest find-tile-test
	(testing "find-tile")
		(def find-tile #'puzzle/find-tile)
		(let [p1 (gen-puzzle 3 3 [1 2 3 4 5 6 7 8 0])
			  p2 (gen-puzzle 2 3 [1 2 3 4 5 0])]
			(is (= (find-tile p1 1) 0))
			(is (= (find-tile p1 4) 3))
			(is (= (find-tile p1 0) 8))
			(is (= (find-tile p2 4) 3))
			(is (= (find-tile p2 0) 5))))

(deftest row-of-tile-test
	(testing "row-of-tile")
		(def row-of-tile #'puzzle/row-of-tile)
		(let [p1 (gen-puzzle 3 3 [1 2 3 4 5 6 7 8 0])
			  p2 (gen-puzzle 3 2 [1 2 3 4 5 0])
			  p3 (gen-puzzle 2 3 [1 2 3 4 5 0])]
			(is (= (row-of-tile p1 1) 0))
			(is (= (row-of-tile p1 3) 0))
			(is (= (row-of-tile p1 4) 1))
			(is (= (row-of-tile p1 5) 1))
			(is (= (row-of-tile p1 0) 2))

			(is (= (row-of-tile p2 1) 0))
			(is (= (row-of-tile p2 2) 0))
			(is (= (row-of-tile p2 3) 1))
			(is (= (row-of-tile p2 0) 2))

			(is (= (row-of-tile p3 1) 0))
			(is (= (row-of-tile p3 2) 0))
			(is (= (row-of-tile p3 3) 0))
			(is (= (row-of-tile p3 4) 1))
			(is (= (row-of-tile p3 0) 1))))

(deftest manhattan-distance-test
	(testing "manhattan-distance")
	    (let [p1 (gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])]
	    	(is (= (manhattan-distance p1) 0))))

			