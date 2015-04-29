(ns npuzzles.astar_naive
  (:use [npuzzles.puzzle])
  (:use [npuzzles.tree_puzzle])
  (:require [taoensso.timbre.profiling :as profiling :refer (pspy pspy* profile defnp p p*)]))

; In the naive implementation of the A* best first search algorithm, we implement the open
; and closed sets as vectors. Membership and insertion for vectors, much like for lists, 
; is O(n). The obvious inefficiency here is apparant for even some 3x3 puzzles
; that are far from the solved state.

; These are helper functions for the naive implementation of the A* best-first
; search algorithm. 
(defn init-queue 
  "Given a puzzle, returns a Priority Queue with one element: a
     TreePuzzle with nil parent, depth g = 0, and appropriate h 
     given by the heuristic function."
     [firstPuzzle heuristic]
     (let [treePuzzle (puzzle-to-tree firstPuzzle 0 nil heuristic)]
        [treePuzzle]))

(defn- insert-queue
  "Given a TreePuzzle and a priority queue of TreePuzzles, inserts the
  TreePuzzle into the priority queue. Helper function for insert-children"
  [tpuzzle pqueue]
  (loop [queue pqueue searched []]
    (if (or (= queue []) (nil? queue) (< (:h tpuzzle) (:h (first queue))))
      (vec (concat (conj searched tpuzzle) queue))
      (let [fst (queue 0)] 
        (recur (vec (rest queue)) (conj searched fst))))))

(defn- dequeue
  "Returns the first item (lowest cost, highest priority TreePuzzle) off the queue."
 [pqueue] 
   (if (= pqueue []) 
   (throw (Throwable. "Invalid Priority Queue"))
   [(pqueue 0) (vec (rest pqueue))]))


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
             [(vec (concat searched others))  true]
             [plist false])
           (recur others (conj searched initial))

        
        ))))))

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
  [pqueue heuristic]
    (loop [open pqueue closed []]
      (if (= pqueue []) 
          (throw (Throwable. "Invalid Priority Queue"))
          (let [result (dequeue open) 
                tpuzzle (result 0)]
      (if (solved? (:puzzle tpuzzle))
          tpuzzle 
          (let [[new-open new-closed] (insert-children (gen-children tpuzzle heuristic) (result 1)
               (conj closed (:puzzle tpuzzle)))]
          (do (recur new-open new-closed))))))))

(defnp solve
  "Given a initial puzzle state, returns the sequence of puzzles needed to go from that 
  puzzle state to the goal state."
  [puzzle heuristic] 
   ;(if (solvable? puzzle)
   (map-solution (step (init-queue puzzle heuristic) heuristic)))
   ;(println "Not Solvable")))