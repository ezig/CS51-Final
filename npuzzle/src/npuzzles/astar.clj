(ns npuzzles.astar
	(:use [npuzzles.puzzle])
    (:use [npuzzles.heap])
    (:require [taoensso.timbre.profiling :as profiling
           :refer (pspy pspy* profile defnp p p*)]))

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

(defnp insert-children 
	"Given a list of TreePuzzles and a priority queue of TreePuzzles, inserts
	 each TreePuzzles in the list into its appropriate position in the priority
	 queue based on its cost value h (lower cost = higher priority).
	 Returns the updated queue."
	[tree-puzzles open closed]
	(loop [puzzles tree-puzzles newqueue open visited closed]
		(if (= puzzles [])
			[newqueue visited]
		(let [puzzle (puzzles 0) newpuzzles (vec (rest puzzles))
         tiles (:tiles puzzle) in-closed (get visited tiles)
         in-open (get open tiles)]
    (if (and (nil? in-closed) (nil? in-open)) 
      (recur newpuzzles (insert-queue puzzle newqueue) visited)
      (if (and in-closed in-open)
        (throw (Throwable. "Breaks Invariant"))
        (if in-closed
          (if (< (:h puzzle) in-closed) 
            (recur newpuzzles (insert-queue puzzle newqueue) (dissoc visited tiles))
            (recur newpuzzles newqueue visited))
          (if (< (:h puzzle) (:h in-open))
            (recur newpuzzles (insert-queue puzzle newqueue) visited)
            (recur newpuzzles newqueue visited)))))))))
               
      
; TODO 

(defnp step 
	"Dequeues a puzzle off the priority queue, checks if it is equal to the
	goal state (by calling solved?). If it is, return the TreePuzzle; if not, insert the child
	nodes into the priority queue and recursively pass the new priority queue into step.
	Thus, step will always return a TreePuzzle that corresponds to the goal state."
	[pqueue]
    (loop [open pqueue closed (hash-map)]
    	(if (= pqueue []) 
       		(throw (Throwable. "Invalid Priority Queue"))
        	(let [result (dequeue open) 
            	  tpuzzle (result 0)]
    	(if (solved? (:puzzle tpuzzle))
        	tpuzzle 
        	(let [[new-open new-closed] (insert-children (gen-children tpuzzle) (result 1) 
               (assoc closed (:tiles (:puzzle tpuzzle)) (:h tpuzzle)))]
       		(do  (recur new-open new-closed))))))))


(comment
(defnp member 
	"Given a tree-puzzle and a vector of tree-puzzles, returns true if there is a puzzle 
	there is a TreePuzzle whose puzzle has the same tile formation as the tiles of the puzzle of
	the TreePuzzle that was passed in"
    [tpuzzle plist]
    (let [puzzle (:puzzle tpuzzle) score (:h tpuzzle)]
    (loop [puzzles plist searched []]
    	(if (= puzzles [])
    		[plist true]
    		(let [initial (puzzles 0) 
    		  	  others (vec (rest puzzles))
    		  	  puzzle2 (:puzzle initial)
    		 	  score2 (:h initial)]
    		(if (= puzzle puzzle2)
    			 (if (< score score2)
    			 	 [(p :cons (vec (concat searched others))) true]
    			 	 [plist false])
    			 (recur others (conj searched initial))

    		
        ))))))

(defn insert-children 
	"Given a list of TreePuzzles and a priority queue of TreePuzzles, inserts
	 each TreePuzzles in the list into its appropriate position in the priority
	 queue based on its cost value h (lower cost = higher priority).
	 Returns the updated queue."
	[tree-puzzles open closed]
	(loop [puzzles tree-puzzles newqueue open visited closed]
		(if (= puzzles [])
			[newqueue visited]
		(let [puzzle (puzzles 0) newpuzzles (vec (rest puzzles)) 
         [new-open update-open] (member puzzle newqueue)
         [new-closed update-open2] (member puzzle visited)]
      (if (or update-open update-open2)
        (recur newpuzzles (insert-queue puzzle new-open) new-closed)
        (recur newpuzzles new-open new-closed)
      )))))
               
      
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
        	(let [result (dequeue open) 
            	  tpuzzle (result 0)]
    	(if (solved? (:puzzle tpuzzle))
        	tpuzzle 
        	(let [[new-open new-closed] (insert-children (gen-children tpuzzle) (result 1) 
               (conj closed (:puzzle tpuzzle)))]
       		(do (recur new-open new-closed))))))))

)

(defn- map-solution
	"Given the return value of solve (a TreePuzzle node thatcorresponds to the goal state), 
	the function returns a list of Puzzles that correspond to the optimal path found by A* 
	by retracing the path back to the starting node (through the Parent nodes of each
    TreePuzzle until you reach the node with the nil parent)."
	[tpuzzle]
  (loop [puz tpuzzle plist ()]
      (if (nil? puz)
          plist
          (recur (:parent puz) (cons (:puzzle puz) plist)))))

; PUBLIC FUNCTIONS
(defn solve
	"Given a initial puzzle state, returns the sequence of puzzles needed to go from that 
	puzzle state to the goal state."
	[puzzle] 
   ;(if (solvable? puzzle)
   (map-solution (step (init-queue puzzle))))
   ;(println "Not Solvable")))