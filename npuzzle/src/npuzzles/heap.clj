(ns npuzzles.heap
	(:use [npuzzles.puzzle]))
 
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
	[{score :h puz :puzzle :as tree-puzzle} pqueue]
	(loop [queue pqueue searched []]
		(if (or (= queue ()) (nil? queue))
			(conj searched tree-puzzle) 
		    (let [first-element (queue 0)
		          others (into [] (rest queue))
		          elem-tiles (:tiles puz)]
			(let [first-score (:h first-element)
				  first-tiles (:tiles(:puzzle first-element))] 
	    		(if (<= score first-score) 
	        		(into [] (concat (conj searched tree-puzzle) queue))
	        		(if (= elem-tiles first-tiles)
	        			(conj searched queue) 
	        			(recur others (conj searched first-element)))))))))

(defn dequeue
	"Returns the first item (lowest cost, highest priority TreePuzzle) off the queue."
 [pqueue] 
   (if (= pqueue []) 
   (throw (Throwable. "Invalid Priority Queue"))
   [(first pqueue) (pop pqueue)])
)

; Private Functions
(defn puzzle-to-tree 
	[puz depth parent]
	(let [distance (+ depth (manhattan-distance puz))]
       {:puzzle puz, :parent parent, :g depth, :h distance}))