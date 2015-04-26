(ns npuzzles.core
	(:require [npuzzles.puzzle :as puzzle])
	(:require [npuzzles.astar :as astar])
	(:require [npuzzles.geneticsolver :as genetic])
  	(:require [npuzzles.heap :as heap])
  	(:gen-class))

(defn -main
	 "I don't do a whole lot ... yet."
 	[& args]
 	(println "Hello, World!")
  (let [x (puzzle/gen-puzzle 2)]
    (map (fn [x] (print-str (puzzle/to-string x))) (astar/solve x))
  ))
