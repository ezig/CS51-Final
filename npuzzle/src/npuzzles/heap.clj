(ns npuzzles.heap
	(:use [npuzzles.puzzle])
	(:require [taoensso.timbre.profiling :as profiling
           :refer (pspy pspy* profile defnp p p*)])
  (:require [clojure.data.priority-map :as pmap]))

 
(comment 
  What we prioritize in our priority queue is insertion, deletion, and 
  extracting the first ezlement. A binary minheap is a very good choice for this,
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

(declare puzzle-to-tree)

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
     (pmap/priority-map-by queue-sort
                             (:tiles puzzle) (puzzle-to-tree puzzle 0 nil))
)

(defnp insert-queue
	"Given a TreePuzzle and a priority queue of TreePuzzles, inserts the
	TreePuzzle into the priority queue. Helper function for insert-children"
  [tpuzzle queue]
  (assoc queue (:tiles (:puzzle tpuzzle)) tpuzzle) 
)

(defn dequeue
	"Returns the first item (lowest cost, highest priority TreePuzzle) off the queue."
 [pqueue] 
 [(second (first pqueue)) (pop pqueue)]
)

(comment 
; Public Functions
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
  [tpuzzle pqueue]
  (loop [queue pqueue searched []]
    (if (or (= queue []) (nil? queue) (< (:h tpuzzle) (:h (first queue))))
      (vec (concat (conj searched tpuzzle) queue))
      (let [fst (queue 0)] 
        (recur (vec (rest queue)) (conj searched fst))))))

(defn dequeue
  "Returns the first item (lowest cost, highest priority TreePuzzle) off the queue."
 [pqueue] 
   (if (= pqueue []) 
   (throw (Throwable. "Invalid Priority Queue"))
   [(pqueue 0) (vec (rest pqueue))])
)
)
; Private Functions
(defn puzzle-to-tree 
  [puz depth parent]
  (let [distance (+ depth (manhattan-distance puz))]
       {:puzzle puz, :parent parent, :g depth, :h distance}))