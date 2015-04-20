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


;PRIVATE FUNCTIONS
(defn- puzzle-to-tree 
	[puzzle depth parent]
	(let [distance (+ depth (puzzle/manhattan-distance puzzle))]
       {:puzzle puzzle, :parent parent, :g depth, :h distance}))

(defn- init-queue 
	"Given a puzzle, returns a Priority Queue with one element: a
     TreePuzzle with nil parent, depth g = 0, and appropriate h 
     given by the heuristic function."
     [firstPuzzle]
     (let [treePuzzle (puzzle-to-tree firstPuzzle 0 nil)]
        [treePuzzle]))

(defn- gen-children 
	"Given a TreePuzzle, generates all possible subsequent puzzle states
     by mapping slide over the result from a puzzle/valid-directions call.
     We then map over the children and remove them if they are equal to the
     puzzles parent node (we dont want to backtrack). Returns a list
     containing the filtered list of child TreePuzzles."
	[{current-state :puzzle, parent :parent, depth :g :as current-tree}]
	     (let [parentTiles (:tiles (:puzzle parent))
	           directions (puzzle/valid-directions current-state)
	           childPuzzles (map #(puzzle/slide current-state %) directions)
	           childPuzzleTrees (map #(puzzle-to-tree % (+ depth 1) current-tree) childPuzzles)]
	(filter #(not= (:tiles (:puzzle %)) parentTiles) childPuzzleTrees)))

(defn- insert-queue
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

(defn- insert-children 
	"Given a list of TreePuzzles and a priority queue of TreePuzzles, inserts
	 each TreePuzzles in the list into its appropriate position in the priority
	 queue based on its cost value h (lower cost = higher priority).
	 Returns the updated queue."
	[tree-puzzles pqueue]
	(loop [puzzles tree-puzzles newqueue pqueue]
		(if (= puzzles ())
			newqueue
		    (let [initial (first puzzles) others (rest puzzles)]
			(recur others (insert-queue initial newqueue))))))
; TODO 
(defn- dequeue
	"Returns the first item (lowest cost, highest priority TreePuzzle) off the queue."
	[pqueue])

(defn- step 
	"Dequeues a puzzle off the priority queue, checks if it is equal to the
	goal state (by calling solved?). If it is, return the TreePuzzle; if not, insert the child
	nodes into the priority queue and recursively pass the new priority queue into step.
	Thus, step will always return a TreePuzzle that corresponds to the goal state."
	[pqueue])

(defn- map-solution
	"Given the return value of solve (a TreePuzzle node thatcorresponds to the goal state), 
	the function returns a list of Puzzles that correspond to the optimal path found by A* 
	by retracing the path back to the starting node (through the Parent nodes of each
    TreePuzzle until you reach the node with the nil parent)."
	[tree-puzzle])

; PUBLIC FUNCTIONS
(defn solve
	"Given a initial puzzle state, returns the sequence of puzzles needed to go from that 
	puzzle state to the goal state."
	[puzzle])

      
