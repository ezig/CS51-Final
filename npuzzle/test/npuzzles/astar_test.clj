(comment (ns npuzzles.astar-test
  (:require [npuzzles.astar :as astar :refer :all]
            [npuzzles.puzzle :as puzzle :refer :all]
            [npuzzles.astar :as astar :refer :all]))


(deftest puzzle-to-tree-test
    (def puzzle-to-tree #'astar/puzzle-to-tree) 
	(testing "puzzle-to-tree")
	(let [p1 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])
	      parent1 (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8])
	      p2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12])
	      parent2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12])]
	 (is (= (puzzle-to-tree p1 1 parent1) 
	 	{:puzzle (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8]), :parent (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8]), :g 1, :h 3}))
	 (is (= (puzzle-to-tree p2 5 parent2) 
	 	{:puzzle (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12]), :parent (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12]), :g 5, :h 33}))))

(deftest gen-children-test
	(def gen-children #'astar/gen-children) 
	(def puzzle-to-tree #'astar/puzzle-to-tree) 
	(testing "gen-children")
	(let [p1 (puzzle/gen-puzzle 3 3 [1 2 3 4 5 6 7 0 8])
	      parent1 (puzzle/gen-puzzle 3 3 [1 2 3 4 0 6 7 5 8])
	      p2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 8 9 10 11 0 13 14 12])
	      parent2 (puzzle/gen-puzzle 4 4 [15 1 2 3 4 5 6 7 0 9 10 11 8 13 14 12])]
	(let [p1-parent (puzzle-to-tree parent1 0 nil)]
    (let [p1-tree (puzzle-to-tree p1 1 p1-parent)
    	  p2-tree (puzzle-to-tree p2 3 parent2)]
    (is (= (gen-children p1-tree) '({:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 5 6 7 8 0]}, 
    	                                   :parent {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 5 6 7 0 8]}, 
    	                                   :parent {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 0 6 7 5 8]}, 
    	                                   :parent nil, :g 0, :h 4}, :g 1, :h 3}, :g 2, :h 2},
    	                                  {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 5 6 0 7 8]}, 
    	                                   :parent {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 5 6 7 0 8]}, 
    	                                   :parent {:puzzle {:rows 3, :cols 3, :tiles [1 2 3 4 0 6 7 5 8]}, 
    	                                   :parent nil, :g 0, :h 4}, :g 1, :h 3}, :g 2, :h 6})))))))

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
	(let [p1-parent (puzzle-to-tree parent1 0 nil)
		  p2-parent (puzzle-to-tree parent2 0 nil)
		  p3-parent (puzzle-to-tree parent3 0 nil)
		  p4-parent (puzzle-to-tree parent4 0 nil)]
    (let [p1-tree (puzzle-to-tree p1 1 p1-parent)
    	  p2-tree (puzzle-to-tree p2 1 p2-parent)
    	  p3-tree (puzzle-to-tree p3 1 p3-parent)
    	  p4-tree (puzzle-to-tree p4 1 p4-parent)]
    (let [pqueue [p1-tree p2-tree] othertrees [p3-tree p4-tree]]
    (let [inserted-puzzles [p3-tree p1-tree p2-tree p4-tree]
    	  actual (insert-children othertrees pqueue)]
    	  (is (= actual inserted-puzzles)))))))))
