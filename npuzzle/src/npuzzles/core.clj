(ns npuzzles.core
	(:require [npuzzles.puzzle :as puzzle])
	(:require [npuzzles.genetic :as genetic])
	(:require [npuzzles.astar :as astar])
	(:require [clojure.data.priority-map :as pmap])
    (:require [clojure.tools.cli :refer [cli]])
	(:gen-class))

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
            (println n)
            (if (zero? n)
                (str "Accuracy: " successes "/" trials)
                (let [puz (puzzle/gen-puzzle (args 0) (args 1))]
                    (if (apply fun (conj (drop 2 args) puz))
                        (recur (dec n) (inc successes))
                        (recur (dec n) successes)))))))

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
                                  (time-tests trials (resolve 'npuzzles.astar/solve) [rows cols heuristic]))))                            
                        (catch Exception e (println "Invalid input."))))
                (if (= "genetic" (:alg opts))
                        (if (not (= (count args) 7))
                            (println "Invalid")
                            (try
                                (let [trials (Integer. (args 0))
                                      rows (Integer. (args 1))
                                      cols (Integer. (args 2))
                                      pop-size (Integer. (args 3))
                                      phases (Integer. (args 4))
                                      gens (Integer. (args 5))
                                      heuristic (resolve (symbol (str "npuzzles.puzzle/" (args 6))))]
                                (if (or (nil? heuristic) (not (:heuristic (meta heuristic))))
                                    (println "Invalid heuristic function.")
                                    (println
                                      (time-tests trials (resolve 'npuzzles.genetic/solve) [rows cols pop-size phases gens heuristic]))))                           
                                (catch Exception e (println "Invalid input."))))
                        (println "Invalid algorithm choice"))))
        (when-not (:data opts)
            (if (not (= (count args) 3))
                (println "Invalid")
                (try 
                    (let [tiles (into [] (map #(- (int %) 48) (args 2)))
                          puzzle (puzzle/gen-puzzle (Integer. (args 0)) (Integer. (args 1)) tiles)]
                        (if (not (puzzle/solvable? puzzle))
                            (println "Unsolvable input!")
                            (println (case (:alg opts)
                                "astar" (astar/solve puzzle puzzle/manhattan-distance)
                                "genetic" (let [solution (genetic/solve puzzle)]
                                        (if (nil? solution)
                                            "Genetic algorithm could not solve puzzle."
                                            solution))
                                "Invalid algorithm choice."))))
                    (catch Exception e (println "Invalid input puzzle")))))))
