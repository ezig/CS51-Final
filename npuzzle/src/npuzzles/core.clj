(ns npuzzles.core
	(:require [npuzzles.puzzle :as puzzle])
	(:require [npuzzles.genetic :as genetic])
	(:require [npuzzles.astar_naive :as naive])
	(:require [npuzzles.astar_efficient :as efficient])
	(:require [clojure.data.priority-map :as pmap])
    (:require [clojure.tools.cli :refer [cli]])
	(:gen-class))
; (defn -main
; 	 "I don't do a whole lot ... yet."
;  	[& args]
;  	(println "\to==+-- 
;             |  |\\ \\
;             |  | \\ \\     _____________________
;             |   \\ \\ \\   |                   |
;             |    \\ \\ \\  |  +-------------+  |
;             |     \\ \\ \\ |  |      (__)   |  |
;             |      \\ \\ \\|  |      (oo)   |  |
;             |       \\ \\  |  | o\\  .\\/.  |  |
;             |        \\ \\ |  | | \\/  \\   |  |
;           /---\\       \\  |  +-------------+  |
;          /     \\       \\ |                   |
;          |     |           |                   |
;          \\     /          |                   |
;           \\---/           |                   |
;                            |                   |
;                      ------------------------------
;                     (                              )
;                      ------------------------------ ")
;  	(println "Under Cow-struction")
  ; (time (dotimes [n 100] (println (efficient/solve (puzzle/gen-puzzle 3 4) puzzle/manhattan-distance))))
;  ;   {:h-weight 9/10, :c-weight 1/10, :cross-weight 9/10,:mut-weight 1/10, :heuristic #'puzzle/misplaced-tiles})))))
(def required-opts #{:alg})

(defn missing-required?
    "Returns true if any required options are not present"
    [opts]
    (not-every? opts required-opts))

(defn time-tests
    ""
    [trials fun args]
    (time 
        (loop [n trials
               successes 0]
            (if (zero? n)
                (str "Accuracy: " successes "/" trials)
                (if (apply fun args)
                    (recur (dec n) (inc successes))
                    (recur (dec n) successes))))))
    ; (let [successes (time (reduce #(if (eval fun) (+ 1 %1) %1) (range trials)))]
    ;     (str "Accuracy: " successes "/" trials)))

(defn -main [& args]
    (let [[opts args banner] 
        (cli args
            ["-h" "--help" "Print help dialog" :default false :flag true]
            ["-d" "--data" "Enables data mode" :default false :flag true]
            ["-a" "--alg" "Required flag Selects algorithm. Valid choices astar or genetic"])]
        (when (or (:help opts) (missing-required? opts))
            (println banner)
            (System/exit 0))
        (when (:data opts)
            (if (= "astar" (:alg opts))
                (if (not (= (count args) 4))
                    (println "Invalid")
                    (try
                        (let [trials (Integer. (args 0))
                              rows (Integer. (args 1))
                              cols (Integer. (args 2))
                              heuristic (resolve (symbol (str "npuzzles.puzzle/" (args 3))))]
                            (if (or (nil? heuristic) (not (:heuristic (meta heuristic))))
                                (println "Invalid heuristic function.")
                                (println
                                  (time-tests trials 'efficient/solve [(puzzle/gen-puzzle rows cols) heuristic]))))                            
                        (catch Exception e (println "Invalid input."))))))
        (when-not (:data opts)
            (if (not (= (count args) 3))
                (println "Invalid")
                (try 
                    (let [tiles (into [] (map #(- (int %) 48) (args 2)))
                          puzzle (puzzle/gen-puzzle (Integer. (args 0)) (Integer. (args 1)) tiles)]
                        (if (not (puzzle/solvable? puzzle))
                            (println "Unsolvable input!")
                            (println (case (:alg opts)
                                "astar" (efficient/solve puzzle puzzle/manhattan-distance)
                                "genetic" (let [solution (genetic/solve puzzle)]
                                        (if (nil? solution)
                                            "Genetic algorithm could not solve puzzle."
                                            solution))
                                "Invalid algorithm choice."))))
                    (catch Exception e (println "Invalid input puzzle")))))))
; 	; ;genetic arity arguments and body
	; ([type trials height width popsize num-phases num-gens & new-params]
	; (let [trials (read-string trials) 
	; 	height (read-string height) 
	; 	width (read-string width)
	; 	popsize (read-string popsize)
	; 	num-phases (read-string num-phases)
	; 	num-gens (read-string num-gens)
	; 	new-params (read-string new-params)]
	; 	(cond 
	; 		(re-matches #"(?i)genetic" type) 
	; 		(dotimes [n trials] (genetic/solve (puzzle/gen-puzzle height width) popsize num-phases num-gens new-params)
	; 		:else "input format should be main (solver type) (# of trials)"
	; 		))))
	; ;astar arity arguments and body
	; ([type trials height width heuristic]
	; (let [trials (read-string trials) 
	; 	height (read-string height) 
	; 	width (read-string width)]
	; 	(cond (re-matches #"(?i)astar" type) 
	; 		(dotimes [n trials] (efficient/solve (puzzle/gen-puzzle height width) heuristic))
	; 		;(do(loop [cnt trials]
	; 		;	(let [x (puzzle/gen-puzzle height width)]
	; 		;		(map (fn [x] (print-str (puzzle/to-string x))) (efficient/solve x))
	; 		;	(recur (- cnt 1)))))
	; 		:else "input format should be main (solver type) (# of trials) (height) (width) then additional parameters"
	; 		))))
