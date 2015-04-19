(ns npuzzles.astar
	(:require [npuzzles.puzzle :as puzzle]))

; Representation of a puzzle for A* search. Each TreePuzzle node is 
; evaluated using a cost function f = g + h, where g is the depth of
; the node (cost of reaching the node) and h is the expected cost of
; reaching the goal state from the current state as determined by 
; some heuristic function. Each puzzle is also aware of its Parent 
; (another TreePuzzle) so that when the goal state is reached, the
; path back to the starting node can be retraced.
(defrecord TreePuzzle [puzzle parent g h])



(defn puzzle-to-tree 
	[puzzle depth parent]
	(let [distance (+ depth (puzzle/manhattan-distance puzzle))]
       {:puzzle puzzle, :parent parent, :g depth, :h distance}))

(defn init-queue 
	"Given a puzzle, returns a Priority Queue with one element: a
     TreePuzzle with nil parent, depth g = 0, and appropriate h 
     given by the heuristic function."
     [firstPuzzle]
     (let [treePuzzle (puzzle-to-tree firstPuzzle 0 nil)]
        [treePuzzle]))

(defn gen-children 
	"Given a TreePuzzle, generates all possible subsequent puzzle states
     by mapping slide over the result from a puzzle/valid-directions call. We then map over the
     children and remove them if they are equal to the puzzles parent node (we dont want
     to backtrack). Returns a list containing the filtered list of child TreePuzzles."
	[{current-state :puzzle, parent :parent, depth :g}]
	     (let [directions (puzzle/valid-directions current-state)]
	     (let [childPuzzles (map #(puzzle/slide current-state %) directions)]
	     (let [childPuzzleTrees (map #(puzzle-to-tree % (+ depth 1) current-state) childPuzzles)]
	(filter #(not= (:puzzle %) parent) childPuzzleTrees)))))

      
