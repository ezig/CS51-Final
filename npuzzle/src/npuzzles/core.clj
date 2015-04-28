(ns npuzzles.core
	(:require [npuzzles.puzzle :as puzzle])
	(:require [npuzzles.genetic :as genetic])
  (:require [npuzzles.astar_naive :as naive])
  (:require [npuzzles.astar_efficient :as efficient])
  (:require [clojure.data.priority-map :as pmap])
  (:require [taoensso.timbre.profiling :as profiling :refer (pspy pspy* profile defnp p p*)])
  (:gen-class))

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
  (println (time (dotimes [n 100] (println n) (if (nil? (genetic/solve (puzzle/gen-puzzle 3) 200 5 500)) (println "X"))))))
  ; (let [x (puzzle/gen-puzzle 2)]
  ;   (map (fn [x] (print-str (puzzle/to-string x))) (efficient/solve x))
  ; ))
