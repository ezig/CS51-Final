(ns npuzzles.puzzle-test
  (:require [clojure.test :refer :all]
            [npuzzles.puzzle :as puzzle :refer :all]))

(deftest abs
	(testing "abs")
		(def abs #'puzzle/abs)
		(is (= (abs 0) 0))
		(is (= (abs 98) 98))
		(is (= (abs -1234) 1234)))

(deftest swap-test
	(testing "swap")
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

(deftest col-of-tile-test
	(testing "col-of-tile")
		(def col-of-tile #'puzzle/col-of-tile)
		(let [p1 (gen-puzzle 3 3 [1 2 3 4 5 6 7 8 0])
	  		  p2 (gen-puzzle 3 2 [1 2 3 4 5 0])
			  p3 (gen-puzzle 2 3 [1 2 3 4 5 0])]
			(is (= (col-of-tile p1 1) 0))
			(is (= (col-of-tile p1 3) 2))
			(is (= (col-of-tile p1 4) 0))
			(is (= (col-of-tile p1 5) 1))
			(is (= (col-of-tile p1 0) 2))

			(is (= (col-of-tile p2 1) 0))
			(is (= (col-of-tile p2 2) 1))
			(is (= (col-of-tile p2 3) 0))
			(is (= (col-of-tile p2 0) 1))

			(is (= (col-of-tile p3 1) 0))
			(is (= (col-of-tile p3 2) 1))
			(is (= (col-of-tile p3 3) 2))
			(is (= (col-of-tile p3 4) 0))
			(is (= (col-of-tile p3 0) 2))))

(deftest inversions-test
	(testing "inversions")
		(def inversions #'puzzle/inversions)
		(let [p1 (gen-puzzle 4 4 [12 1 10 2 7 11 4 14 5 0 9 15 8 13 6 3])
			  p2 (gen-puzzle 3 3 [7 1 2 5 0 9 8 3 6])
			  p3 (gen-puzzle 3 5 [6 1 2 4 5 7 3 0 14 9 11 12 8 13 10])
			  p4 (gen-puzzle 3 3 [1 2 3 4 5 6 7 8 0])
			  p5 (gen-puzzle 2 3 [1 2 3 4 5 0])]
			(is (= (inversions p1) 49))
			(is (= (inversions p2) 11))
			(is (= (inversions p3) 20))
			(is (= (inversions p4) 0))
			(is (= (inversions p5) 0))))

(deftest slide-test
	(testing "slide")
		(let [p1 (gen-puzzle 3 3 [7 1 2 5 0 9 8 3 6])
	  		  p2 (gen-puzzle 3 2 [1 2 3 4 5 0])]
	  		(is (= (:tiles (slide p1 :up)) [7 1 2 5 3 9 8 0 6]))
	  		(is (= (:tiles (slide p1 :down)) [7 0 2 5 1 9 8 3 6]))
	  		(is (= (:tiles (slide p1 :left)) [7 1 2 5 9 0 8 3 6]))
	  		(is (= (:tiles (slide p1 :right)) [7 1 2 0 5 9 8 3 6]))
	  		(is (= (:tiles (slide p2 :right)) [1 2 3 4 0 5]))
	  		(is (= (:tiles (slide p2 :left)) (:tiles p2)))
			(is (= (:tiles (slide p2 :up)) (:tiles p2)))
			(is (= (:tiles (slide p2 :down)) [1 2 3 0 5 4]))))

(deftest solvable?-test
	(testing "solvable?")
		;(def solvable? #'puzzle/solvable?)
		(let [p1 (gen-puzzle 4 4 [12 1 10 2 7 11 4 14 5 0 9 15 8 13 6 3])
			  p2 (gen-puzzle 3 3 [7 1 2 5 0 9 8 3 6])
			  p3 (gen-puzzle 3 5 [6 1 2 4 5 7 3 0 14 9 11 12 8 13 10])
			  p4 (gen-puzzle 3 3 [1 2 3 4 5 6 7 8 0])
			  p5 (gen-puzzle 2 3 [1 2 3 4 5 0])]
			  (is (solvable? p1))
			  (is (not (solvable? p2)))
			  (is (solvable? p3))
			  (is (solvable? p4))
			  (is (solvable? p5))))

(deftest manhattan-distance-test
	(testing "manhattan-distance")
	    (let [p1 (gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])
	    	  p2 (gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12])
	    	  p3 (gen-puzzle 3 3 [2 1 3 5 4 0 6 7 8])
	    	  p4 (gen-puzzle 3 3 [6 4 7 8 5 0 3 2 1])
	    	  p5 (gen-puzzle 4 4 [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0])]
	    	(is (= (manhattan-distance p1) 2))
	    	(is (= (manhattan-distance p2) 28))
	    	(is (= (manhattan-distance p3) 10))
	    	(is (= (manhattan-distance p4) 22))
	    	(is (= (manhattan-distance p5) 0))))

(deftest solved?-test
	(testing "solved?")
		(let [p1 (gen-puzzle 4 4 [12 1 10 2 7 11 4 14 5 0 9 15 8 13 6 3])
			  p2 (gen-puzzle 3 3 [7 1 2 5 0 9 8 3 6])
			  p3 (gen-puzzle 3 5 [6 1 2 4 5 7 3 0 14 9 11 12 8 13 10])
			  p4 (gen-puzzle 3 3 [1 2 3 4 5 6 7 8 0])
			  p5 (gen-puzzle 2 3 [1 2 3 4 5 0])]
			  (is (not (solved? p1)))
			  (is (not (solved? p2)))
			  (is (not (solved? p3)))
			  (is (solved? p4))
			  (is (solved? p5))))