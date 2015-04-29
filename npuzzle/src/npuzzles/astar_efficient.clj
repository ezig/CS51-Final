(ns npuzzles.astar_efficient
  (:use [npuzzles.puzzle])
  (:use [npuzzles.tree_puzzle])
  (:require [clojure.data.priority-map :as pmap])
  (:require [taoensso.timbre.profiling :as profiling :refer (pspy pspy* profile defnp p p*)]))
  

; Here we improve apon the naive implementation on the A* best-first search algorithm
; by altering the datastructures that we implement the open and closed sets as. 

(defn- queue-sort 
  [val1 val2] 
  (if (= (:h val1) (:h val2)) 
    (< (read-string (clojure.string/join (cons 1 (:tiles (:puzzle val1)))))
       (read-string (clojure.string/join (cons 1 (:tiles (:puzzle val2))))))
    (< (:h val1) (:h val2))))
  

;Public Functions
(defn init-queue 
  "Given a puzzle, returns a Priority Queue with one element: a
     TreePuzzle with nil parent, depth g = 0, and appropriate h 
     given by the heuristic function."
     [puzzle]
     (pmap/priority-map-by queue-sort (:tiles puzzle) (puzzle-to-tree puzzle 0 nil)))

(defn- insert-queue
  "Given a TreePuzzle and a priority queue of TreePuzzles, inserts the
  TreePuzzle into the priority queue. Helper function for insert-children"
  [tpuzzle queue]
  (assoc queue (:tiles (:puzzle tpuzzle)) tpuzzle))

(defn- dequeue
  "Returns the first item (lowest cost, highest priority TreePuzzle) off the queue."
 [pqueue] 
 [(second (first pqueue)) (pop pqueue)])

(defn- insert-children 
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

(defnp solve
  "Given a initial puzzle state, returns the sequence of puzzles needed to go from that 
  puzzle state to the goal state."
  [puzzle] 
   ;(if (solvable? puzzle)
   (map-solution (step (init-queue puzzle))))
   ;(println "Not Solvable")))

