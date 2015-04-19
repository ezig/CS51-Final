(ns npuzzles.astar-test
  (:require [clojure.test :refer :all]
            [npuzzles.puzzle :as puzzle :refer :all]
            [npuzzles.astar :as astar :refer :all]))


(deftest puzzle-to-tree-test 
	(testing "puzzle-to-tree")
	(let [p1 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])
	      parent1 (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8])
	      p2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12])
	      parent2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12])]
	 (is (= (astar/puzzle-to-tree p1 1 parent1) 
	 	{:puzzle (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8]), :parent (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8]), :g 1, :h 3}))
	 (is (= (astar/puzzle-to-tree p2 5 parent2) 
	 	{:puzzle (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12]), :parent (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12]), :g 5, :h 33}))))

(deftest gen-children-test
	(testing "gen-children")
	(let [p1 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])
	      parent1 (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8])
	      p2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12])
	      parent2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12])]
    (let [p1-tree (astar/puzzle-to-tree p1 1 parent1)
    	  p2-tree (astar/puzzle-to-tree p2 3 parent2)]
    (is (not= (astar/gen-children p1-tree) '({(puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 0 7 8]),(puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8]),2,6},
    	                                 {(puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 8 0]),(puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8]),2,2}))))))