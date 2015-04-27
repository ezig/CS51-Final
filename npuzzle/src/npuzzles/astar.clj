(ns npuzzles.astar
  (:use [npuzzles.tree_puzzle])
  (:require [npuzzles.astar_naive :as naive])
  (:require [npuzzles.astar_efficient :as efficient])
  (:require [taoensso.timbre.profiling :as profiling :refer (pspy pspy* profile defnp p p*)]))

; PUBLIC FUNCTIONS
(defnp solve
  "Given a initial puzzle state, returns the sequence of puzzles needed to go from that 
  puzzle state to the goal state."
  [puzzle] 
   ;(if (solvable? puzzle)
   (map-solution (efficient/step (efficient/init-queue puzzle))))
   ;(println "Not Solvable")))