(ns npuzzles.astar-test
  (:require [clojure.test :refer :all]
  			[npuzzles.astar :as astar :refer :all]
            [npuzzles.puzzle :as puzzle :refer :all]
            [npuzzles.astar :as astar :refer :all]
            [clojure.data.priority-map :as pmap]))


(deftest puzzle-to-tree-test
    (def puzzle-to-tree #'astar/puzzle-to-tree) 
	(testing "puzzle-to-tree")
	(let [p1 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])
	      parent1 (puzzle-to-tree (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8]) 0 nil puzzle/manhattan-distance)
	      p2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12])
	      parent2 (puzzle-to-tree (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12]) 0 nil puzzle/manhattan-distance)]
	 (is (= (puzzle-to-tree p1 1 parent1 puzzle/manhattan-distance) 
	 	{:puzzle p1, :parent parent1, :g 1, :h 2}))
	 (is (= (puzzle-to-tree p2 1 parent2 puzzle/manhattan-distance) 
	 	{:puzzle p2, :parent parent2, :g 1, :h 26}))))

(deftest gen-children-test
	(def gen-children #'astar/gen-children) 
	(def puzzle-to-tree #'astar/puzzle-to-tree) 
	(testing "gen-children")
	(let [p1 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])
	      parent1 (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8])
	      p2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12])
	      parent2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12])]
	(let [p1-parent (puzzle-to-tree parent1 0 nil puzzle/manhattan-distance)]
    (let [p1-tree (puzzle-to-tree p1 1 p1-parent puzzle/manhattan-distance)
    	  p2-tree (puzzle-to-tree p2 3 parent2 puzzle/manhattan-distance)]
    (is (= (gen-children p1-tree puzzle/manhattan-distance) [{:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 5 6 7 8 0]}, 
    	                                   :parent {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 5 6 7 0 8]}, 
    	                                   :parent {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 0 6 7 5 8]}, 
    	                                   :parent nil, :g 0, :h 2}, :g 1, :h 2}, :g 2, :h 2},
    	                                  {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 5 6 0 7 8]}, 
    	                                   :parent {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 5 6 7 0 8]}, 
    	                                   :parent {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 0 6 7 5 8]}, 
    	                                   :parent nil, :g 0, :h 2}, :g 1, :h 2}, :g 2, :h 4}]))))))
