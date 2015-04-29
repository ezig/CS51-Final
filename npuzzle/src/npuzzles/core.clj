(ns npuzzles.core
	(:require [npuzzles.puzzle :as puzzle])
	(:require [npuzzles.genetic :as genetic])
	(:require [npuzzles.astar_naive :as naive])
	(:require [npuzzles.astar_efficient :as efficient])
	(:require [clojure.data.priority-map :as pmap])
	(:require [taoensso.timbre.profiling :as profiling :refer (pspy pspy* profile defnp p p*)])
	(:gen-class))

(comment
(defn -main
	 "I don't do a whole lot ... yet."
 	[& args]
 	(println "\to==+-- 
            |  |\\ \\
            |  | \\ \\     _____________________
            |   \\ \\ \\   |                   |
            |    \\ \\ \\  |  +-------------+  |
            |     \\ \\ \\ |  |     (__)    |  |
            |      \\ \\ \\|  |     (oo)    |  |
            |       \\ \\  |  | o\\  .\\/.  |  |
            |        \\ \\ |  | | \\/    \\ |  |
          /---\\       \\  |  +-------------+  |
         /     \\       \\ |                   |
         |     |           |                   |
         \\     /          |                   |
          \\---/           |                   |
                           |                   |
                     ------------------------------
                    (                              )
                     ------------------------------ ")
 	(println "Under Cow-struction")
  (let [x (puzzle/gen-puzzle 2)]
    (map (fn [x] (print-str (puzzle/to-string x))) (efficient/solve x))
  )))
(defn main
	"commandline interaction with our program"
	[type trials height width]
	(cond 
		(re-matches #"(?i)genetic" type) 
		(do (loop [cnt trials]
			(let [x (puzzle/gen-puzzle height width)]
				(map (fn [x] (print-str (puzzle/to-string x))) (genetic/solve x))
			(recur (- cnt 1)))))
		(re-matches #"(?i)astar" type) 
		(do(loop [cnt trials]
			(let [x (puzzle/gen-puzzle height width)]
				(map (fn [x] (print-str (puzzle/to-string x))) (efficient/solve x))
			(recur (- cnt 1)))))
		:else "input format should be main (solver type) (# of trials)"
		)
	)