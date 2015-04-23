(ns npuzzles.heap
	(:require [npuzzles.puzzle :as p]))
 
(comment 
  What we prioritize in our priority queue is insertion, deletion, and 
  extracting the first element. A binary minheap is a very good choice for this,
  providing O(1) insertion and O(logn) deletion. 
)
  
(comment
  Representation of a puzzle for A* search. Each TreePuzzle node is 
  evaluated using a cost function f = g + h, where g is the depth of
  the node (cost of reaching the node) and h is the expected cost of 
  reaching the goal state from the current state as determined by 
  some heuristic function. Each puzzle is also aware of its Parent 
  (another TreePuzzle) so that when the goal state is reached, the
  path back to the starting node can be retraced.
)

(defrecord TreePuzzle [puzzle parent g h])

; Public Functions
(declare puzzle-to-tree)
(defn init-queue 
	"Given a puzzle, returns a Priority Queue with one element: a
     TreePuzzle with nil parent, depth g = 0, and appropriate h 
     given by the heuristic function."
     [firstPuzzle]
     (let [treePuzzle (puzzle-to-tree firstPuzzle 0 nil)]
        [treePuzzle]))

(defn insert-queue
	"Given a TreePuzzle and a priority queue of TreePuzzles, inserts the
	TreePuzzle into the priority queue. Helper function for insert-children"
	[{score :h :as tree-puzzle} pqueue]
	(loop [queue pqueue searched []]
		(if (= queue ()) 
			(conj searched tree-puzzle) 
		    (let [first-element (first queue) others (rest queue)
			      first-score (:h first-element)] 
	    		(if (< score first-score)
	        		(into [] (concat (conj searched tree-puzzle) queue))
	        		(recur others (conj searched first-element)))))))

(defn dequeue
	"Returns the first item (lowest cost, highest priority TreePuzzle) off the queue."
	[pqueue])

; Private Functions
(defn- puzzle-to-tree 
	[puzzle depth parent]
	(let [distance (+ depth (p/manhattan-distance puzzle))]
       {:puzzle puzzle, :parent parent, :g depth, :h distance}))