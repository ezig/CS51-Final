(ns npuzzles.astar
	(:use [npuzzles.puzzle])
    (:use [npuzzles.heap]))

;PRIVATE FUNCTIONS
(defn gen-children 
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
	(vec (filter #(not= (:tiles (:puzzle %)) parentTiles) childPuzzleTrees))))
(defn member 
	"Given a tree-puzzle and a vector of tree-puzzles, returns true if there is a puzzle 
	there is a TreePuzzle whose puzzle has the same tile formation as the tiles of the puzzle of
	the TreePuzzle that was passed in"
    [puz closed]
    (let [tiles (:tiles (:puzzle puz)) score (:h puz)]
    (loop [puzzles closed searched []]
    	(if (= puzzles ())
    		closed
    		(let [initial (puzzles 0) 
    		  	  others (vec (rest puzzles))
    		  	  tiles2 (:tiles (:puzzle initial))
    		 	  score2 (:h initial)]
    		(if (= tiles tiles2)
    			 (if (< score score2)
    			 	 (conj searched others)
    			 	 (conj closed "dummy"))
    			 (recur others (conj searched initial))

    		
        ))))))

(defn insert-children 
	"Given a list of TreePuzzles and a priority queue of TreePuzzles, inserts
	 each TreePuzzles in the list into its appropriate position in the priority
	 queue based on its cost value h (lower cost = higher priority).
	 Returns the updated queue."
	[tree-puzzles open closed]
	(loop [puzzles tree-puzzles newqueue open closed closed]
		(if (= puzzles ())
			[newqueue closed]
		    (let [initial (puzzles 0) 
		    	  others (vec (rest puzzles))
		    	  closed-len (count closed)
		    	  new-closed (member initial closed)
		    	  dont-insert (> (count new-closed) closed-len)]
		    (if dont-insert
		    	(recur others newqueue new-closed)
		    	(recur others (insert-queue initial newqueue) new-closed))))))
; TODO 

(defn step 
	"Dequeues a puzzle off the priority queue, checks if it is equal to the
	goal state (by calling solved?). If it is, return the TreePuzzle; if not, insert the child
	nodes into the priority queue and recursively pass the new priority queue into step.
	Thus, step will always return a TreePuzzle that corresponds to the goal state."
	[pqueue]
    (loop [open pqueue closed []]
    	(if (= pqueue []) 
       		(throw (Throwable. "Invalid Priority Queue"))
        	(let [result (dequeue pqueue) 
            	  tpuzzle (result 0)
              	  newq (result 1)
              	  closed (conj closed tpuzzle)]
    	(if (solved? (:puzzle tpuzzle))
        	tpuzzle 
        	(let [result-insert (insert-children (gen-children tpuzzle) open closed)
        		  new-open (result-insert 0)
        		  new-closed (result-insert 1)] 
       		(do (println str "\n" (to-string (:puzzle tpuzzle))) (recur new-open new-closed))))))))
       

(defn- map-solution
	"Given the return value of solve (a TreePuzzle node thatcorresponds to the goal state), 
	the function returns a list of Puzzles that correspond to the optimal path found by A* 
	by retracing the path back to the starting node (through the Parent nodes of each
    TreePuzzle until you reach the node with the nil parent)."
	[tpuzzle]
  (rseq (reduce (fn [x y] (if (= (:parent x) nil) y (conj y (:puzzle x)))) tpuzzle [])))

; PUBLIC FUNCTIONS
(defn solve
	"Given a initial puzzle state, returns the sequence of puzzles needed to go from that 
	puzzle state to the goal state."
	[puzzle] 
   (if (solvable? puzzle)
   (map-solution (step (init-queue puzzle)))
   (println "Not Solvable")))