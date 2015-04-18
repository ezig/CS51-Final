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

(defn init-queue 
	"Given a puzzle, returns a Priority Queue with one element: a
     TreePuzzle with nil parent, depth g = 0, and appropriate h 
     given by the heuristic function."
     [firstPuzzle]
     (let [d (puzzle/manhattan-distance firstPuzzle)]
        [{:puzzle firstPuzzle, :parent nil, :g 0, :h d}]))
      
