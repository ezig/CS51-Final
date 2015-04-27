(ns npuzzles.core
	(:require [npuzzles.puzzle :as puzzle])
	(:require [npuzzles.astar :as astar])
	(:require [npuzzles.genetic :as genetic])
  	(:require [npuzzles.heap :as heap])
  	(:gen-class))

(defn -main
	 "I don't do a whole lot ... yet."
 	[& args]
 	(println "\t    o==+-- 
            |  |\\ \\
            |  | \\ \\     ____________________
            |   \\ \\ \\   |                  |
            |    \\ \\ \\  |  +------------+  |
            |     \\ \\ \\ |  |     (__)   |  |
            |      \\ \\ \\|  |     (oo)   |  |
            |       \\ \\ |  | o\\  .\\/.   |  |
            |        \\ \\|  | | \\/    \\  |  |
          /---\\       \\ |  +------------+  |
         /     \\       \\|                  |
         |     |        |                  |
         \\     /        |                  |
          \\---/         |                  |
                        |                  |
                     --------------------------
                    (                          )
                     --------------------------")
 	(println "Under Cow-struction")
  (let [x (puzzle/gen-puzzle 2)]
    (map (fn [x] (print-str (puzzle/to-string x))) (astar/solve x))
  ))
