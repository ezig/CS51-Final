(ns npuzzles.tree_puzzle
  (:use [npuzzles.puzzle])
  (:require [taoensso.timbre.profiling :as profiling
           :refer (pspy pspy* profile defnp p p*)]))

;PRIVATE FUNCTIONS
(defrecord TreePuzzle [puzzle parent g h])

(defn puzzle-to-tree 
  [puz depth parent hueristic]
  (let [distance (+ depth (hueristic puz))]
       {:puzzle puz, :parent parent, :g depth, :h distance}))

(defn gen-children 
  "Given a TreePuzzle, generates all possible subsequent puzzle states
     by mapping slide over the result from a puzzle/valid-directions call.
     We then map over the children and remove them if they are equal to the
     puzzles parent node (we dont want to backtrack). Returns a list
     containing the filtered list of child TreePuzzles."
  [{current-state :puzzle, parent :parent, depth :g :as current-tree} hueristic]
       (let [parentTiles (:tiles (:puzzle parent))
             directions (valid-directions current-state)
             childPuzzles (map #(slide current-state %) directions)
             childPuzzleTrees (map #(puzzle-to-tree % (+ depth 1) current-tree hueristic) childPuzzles)]
        (vec (filter #(not= (:tiles (:puzzle %)) parentTiles) childPuzzleTrees))))

(defn map-solution
  "Given the return value of solve (a TreePuzzle node thatcorresponds to the goal state), 
  the function returns a list of Puzzles that correspond to the optimal path found by A* 
  by retracing the path back to the starting node (through the Parent nodes of each
    TreePuzzle until you reach the node with the nil parent)."
  [tpuzzle]
  (loop [puz tpuzzle dlist ()]
      (if (nil? (:parent puz))
          dlist
          (recur (:parent puz) (cons (dir-between (:puzzle (:parent puz)) (:puzzle puz)) dlist)))))