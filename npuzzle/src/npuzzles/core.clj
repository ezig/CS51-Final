(ns npuzzles.core
	(:require [npuzzles.puzzle :as puzzle])
	(:require [npuzzles.genetic :as genetic])
	(:require [npuzzles.astar_naive :as naive])
	(:require [npuzzles.astar_efficient :as efficient])
	(:require [clojure.data.priority-map :as pmap])
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
            |     \\ \\ \\ |  |      (__)   |  |
            |      \\ \\ \\|  |      (oo)   |  |
            |       \\ \\  |  | o\\  .\\/.  |  |
            |        \\ \\ |  | | \\/  \\   |  |
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
  ; (time (dotimes [n 100] (println (genetic/solve (puzzle/gen-puzzle 3) 200 5 500 {:h-weight 0.9, :c-weight 0.1, :cross-weight 0.9,
  ;                :mut-weight 0.1, :heuristic puzzle/misplaced-tiles})))))
))

(defn -main
	"commandline interaction with our program"
	;genetic arity arguments and body
	[type trials height width popsize num-phases num-gens new-params]
	(let [trials (read-string trials) 
		height (read-string height) 
		width (read-string width)
		popsize (read-string popsize)
		num-phases (read-string num-phases)
		num-gens (read-string num-gens)
		new-params (read-string new-params)]
		(cond 
			(re-matches #"(?i)genetic" type) 
			(dotimes [n trials] (genetic/solve (puzzle/gen-puzzle height width) popsize num-phases num-gens new-params)
			;(do (loop [cnt trials]
			;	(let [x (puzzle/gen-puzzle height width)]
			;		(map (fn [x] (print-str (puzzle/to-string x))) (genetic/solve x))
			;	(recur (- cnt 1)))))
			:else "input format should be main (solver type) (# of trials)"
			))
	;astar arity arguments and body
	[type trials height width heuristic]
	(let [trials (read-string trials) 
		height (read-string height) 
		width (read-string width)]
		(cond (re-matches #"(?i)astar" type) 
			(do(loop [cnt trials]
				(let [x (puzzle/gen-puzzle height width)]
					(map (fn [x] (print-str (puzzle/to-string x))) (efficient/solve x))
				(recur (- cnt 1)))))
			:else "input format should be main (solver type) (# of trials) (height) (width) then additional parameters"
			))	
		)
	)