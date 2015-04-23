(ns npuzzles.astar
	(:use [npuzzles.puzzle])
  (:use [npuzzles.heap]))

(defrecord TreePuzzle [puzzle parent g h])


;PRIVATE FUNCTIONS
(defn- gen-children 
	"Given a TreePuzzle, generates all possible subsequent puzzle states
     by mapping slide over the result from a puzzle/valid-directions call.
     We then map over the children and remove them if they are equal to the
     puzzles parent node (we dont want to backtrack). Returns a list
     containing the filtered list of child TreePuzzles."
	[{current-state :puzzle, parent :parent, depth :g :as current-tree}]
	     (let [parentTiles (:tiles (:puzzle parent))
	           directions (valid-directions current-state)
	           childPuzzles (map #(slide current-state %) directions)
	           childPuzzleTrees (map #(puzzle-to-tree % (+ depth 1) current-tree) childPuzzles)]
	(filter #(not= (:tiles (:puzzle %)) parentTiles) childPuzzleTrees)))

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

      
