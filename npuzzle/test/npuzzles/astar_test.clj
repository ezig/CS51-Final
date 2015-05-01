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
	      parent1 (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8])
	      p2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12])
	      parent2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12])]
	 (is (= (puzzle-to-tree p1 1 parent1 puzzle/manhattan-distance) 
	 	{:puzzle p1, :parent parent1, :g 1, :h 2}))
	 (is (= (puzzle-to-tree p2 5 parent2 puzzle/manhattan-distance) 
	 	{:puzzle p2, :parent parent2, :g 5, :h 31}))))

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

(deftest insert-children-test 
	(def insert-children #'astar/insert-children) 
	(def puzzle-to-tree #'astar/puzzle-to-tree)
	(testing "insert-children")
	(let [p1 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])
	      parent1 (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8])
	      p2 (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8])
	      parent2 (puzzle/gen-puzzle 3 3 [1 0 3 4 2 6 1 5 8])
	      p3 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 8 0])
	      parent3 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 0 7 8 6])
	      p4 (puzzle/gen-puzzle 3 3 [1 2 0 4 5 3 7 8 6])
	      parent4 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 0 7 8 6])]
	(let [p1-parent (puzzle-to-tree parent1 0 nil puzzle/manhattan-distance)
		  p2-parent (puzzle-to-tree parent2 0 nil puzzle/manhattan-distance)
		  p3-parent (puzzle-to-tree parent3 0 nil puzzle/manhattan-distance)
		  p4-parent (puzzle-to-tree parent4 0 nil puzzle/manhattan-distance)]
    (let [p1-tree (puzzle-to-tree p1 1 p1-parent puzzle/manhattan-distance)
    	  p2-tree (puzzle-to-tree p2 1 p2-parent puzzle/manhattan-distance)
    	  p3-tree (puzzle-to-tree p3 1 p3-parent puzzle/manhattan-distance)
    	  p4-tree (puzzle-to-tree p4 1 p4-parent puzzle/manhattan-distance)]
    (let [pqueue (pmap/priority-map [1 2 3 4 5 6 7 0 8] p1-tree [ 1 2 3 4 0 6 7 5 8] p2-tree) 
    	  othertrees [p3-tree p4-tree]
    	  closed (pmap/priority-map)] 
    (let [inserted-puzzles (pmap/priority-map [1 2 3 4 5 6 7 8 0] p3-tree [1 2 3 4 5 6 7 0 8] p1-tree
     										  [1 2 3 4 0 6 7 5 8] p2-tree [1 2 3 4 5 0 7 8 6] p4-tree)
    	  actual (insert-children othertrees pqueue closed)]
    	  (is (= actual inserted-puzzles))))))))
