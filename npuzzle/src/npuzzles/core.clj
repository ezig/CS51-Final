(ns npuzzles.core
	(:require [npuzzles.puzzle :as puzzle])
	(:require [npuzzles.genetic :as genetic])
	(:require [npuzzles.astar :as astar])
    (:require [clojure.tools.cli :refer [cli]])
	(:gen-class))

(def required-opts #{:alg})

(defn missing-required?
    "Returns true if any required options are not present"
    [opts]
    (not-every? opts required-opts))

(defn- validate-puzzle
    "Raises an exception if the input parameters for puzzle are Invalid
    (the requirements are that the number of rows
    and columns correspond to the number of tiles in the puzzle, that
    the puzzle is at least of dimension 2 in each direction, and that the only
    tiles in the puzzle are the tiles in the range [0, rows * cols - 1])"
    [rows cols tiles]
    (if (and 
            (= (* rows cols) (count tiles))
            (> rows 1)
            (> cols 1)
            (range 0 (count tiles)) (sort tiles))
        nil
        (throw (Exception. "Invalid puzzle construction."))))

(defn time-tests
    "Given a number of trials, a solving function, and a list of arguments to 
    the solving function (the first two of which must be row/column arguments
    to a puzzle-generating function)
     prints out the total time and the accuracy of the solver"
    [trials fun args]
    (time 
        (loop [n trials
               successes 0]
            (if (zero? n)
                (str "Accuracy: " successes "/" trials)
                (let [puz (puzzle/gen-puzzle (args 0) (args 1))]
                    (if (apply fun (conj (drop 2 args) puz))
                        (recur (dec n) (inc successes))
                        (recur (dec n) successes)))))))

(defn -main [& args]
    "Command line interface for the puzzle solver (see the readme for more detail"
    (let [[opts args banner] 
        (cli args
            ["-h" "--help" "Print help dialog" :default false :flag true]
            ["-d" "--data" "Enables data mode" :default false :flag true]
            ["-a" "--alg" "Required flag Selects algorithm. Valid choices astar or genetic"])]
        ;If help flag is triggered or not enough arguments, print banner
        (when (or (:help opts) (missing-required? opts))
            (println banner)
            (System/exit 0))
        ;-d flag entered, trigger data mode
        (when (:data opts)
            (if (= "astar" (:alg opts))
                (if (not (= (count args) 4))
                    (println "Invalid arguments to astar (see readme)")
                    (try
                        (let [trials (Integer. (args 0))
                              rows (Integer. (args 1))
                              cols (Integer. (args 2))
                              heuristic (resolve (symbol (str "npuzzles.puzzle/" (args 3))))]
                            (validate-puzzle rows cols (range (* rows cols)))
                            (if (or (nil? heuristic) (not (:heuristic (meta heuristic))))
                                (println "Invalid heuristic function.")
                                (println
                                  (time-tests trials (resolve 'npuzzles.astar/solve) [rows cols heuristic]))))                            
                        (catch Exception e (println "Invalid input."))))
                (if (= "genetic" (:alg opts))
                        (if (not (= (count args) 7))
                            (println "Invalid input to genetic (see readme)")
                            (try
                                (let [trials (Integer. (args 0))
                                      rows (Integer. (args 1))
                                      cols (Integer. (args 2))
                                      pop-size (Integer. (args 3))
                                      phases (Integer. (args 4))
                                      gens (Integer. (args 5))
                                      heuristic (resolve (symbol (str "npuzzles.puzzle/" (args 6))))]
                                (validate-puzzle rows cols (range (* rows cols)))
                                (if (or (nil? heuristic) (not (:heuristic (meta heuristic))))
                                    (println "Invalid heuristic function.")
                                    (println
                                      (time-tests trials (resolve 'npuzzles.genetic/solve) [rows cols pop-size phases gens heuristic]))))                           
                                (catch Exception e (println "Invalid input to genetic (see readme)"))))
                        (println "Invalid algorithm choice"))))
        ; No -d flag, puzzle solving mode
        (when-not (:data opts)
            (if (not (= (count args) 3))
                (println "Invalid input to puzzle solve mode (see readme)")
                ; Try to convert the input into a puzzle and solve it
                (try 
                    (let [tiles (into [] (map #(- (int %) 48) (args 2)))
                          rows (Integer. (args 0))
                          cols (Integer. (args 1))
                          puzzle (puzzle/gen-puzzle rows cols tiles)]
                        (validate-puzzle rows cols tiles)
                        (if (not (puzzle/solvable? puzzle))
                            (println "Unsolvable input!")
                            (println (case (:alg opts)
                                "astar" (astar/solve puzzle puzzle/manhattan-distance)
                                "genetic" (let [solution (genetic/solve puzzle)]
                                        (if (nil? solution)
                                            "Genetic algorithm failed to solve puzzle – try again!"
                                            solution))
                                "Invalid algorithm choice."))))
                    (catch Exception e (println "Invalid input puzzle")))))))
