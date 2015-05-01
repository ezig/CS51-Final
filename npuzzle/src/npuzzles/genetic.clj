; genetic.clj
; Provides a genetic algorithm to solve sliding tile puzzles
;
; Some naming conventions used in the file that do not correspond to 
; formally defined data types:
;
; GENE: a floating point number in (0, 1]
; CHROMOSOME: a vector of genes
; POPULATION: a vector of chromosomes
; FITNESS: the score that fitness returns for a chromosome relative to a given
; puzzle. Lower score => more fit
; COST: the length of a solution (number of slides)
;
; A chromosome is a list of floating point numbers that must be interpreted
; in the context of a particular puzzle because the list of valid moves 
; changes relative to the state of the puzzle. Thus, given a particular
; state with an associated list of valid moves, each gene represents which
; of those moves to take. For example, if a given puzzle state has two valid 
; moves (:up, :down), then a gene in [0.0, 0.5) would represent up and 
; a gene in [0.5 , 1.0) would represent down. The genetic algorithm prevents 
; immediate backtracking (sliding the puzzle left right after sliding right),
; so any puzzle state has either 1 (empty square in corner), 2 (empty square
; on the edge) and 3 (empty square in the middle) valid moves.

(ns npuzzles.genetic
	(:use [npuzzles.puzzle])
	(:require [clojure.math.numeric-tower :as math])
	(:require clojure.core.memoize)
    (:require [taoensso.timbre.profiling :as profiling
           :refer (pspy pspy* profile defnp p p*)]))

; Record type for specifying genetic algorithm parameters;
;
; Fields:
; H-WEIGHT: a ratio type in [0, 1] representing the relative weighting of the
; heuristic function in determining the fitness of a solution.
; C-WEIGHT: a ratio in [0, 1] representing the relative weighting of the
; cost in determining the fitness of a solution. 
; NOTE: h-weight + c-weight must equal 1. Solve enforces this invariant using
; validate-params.
;
; CROSS-WEIGHT: a ratio type in [0, 1] representing the relative weighting of
; crossover when building a new population in run-generation
; MUT-WEIGHT: a ratio in [0, 1] representing the relative weighting of the
; mutation when building a new population in run-generation
; NOTE: cross-weight + height-weight must equal 1. Solve enforces this
; invariant using validate-params.
;
; HEURISTIC: a puzzle -> int function used in fitness evaluation.
; This function must have a heuristic metadata tag in order for
; validate-params to consider it a valid heuristic function.

(defrecord GAParams [h-weight c-weight cross-weight mut-weight heuristic])

; Default parameter values
(def ^:dynamic params {:h-weight 9/10, :c-weight 1/10, :cross-weight 9/10,
			           :mut-weight 1/10, :heuristic manhattan-distance})

;PUBLIC FUNCTIONS
(declare run-phase)
(declare interpret-chromosome)
(declare validate-params)
(declare optimal-settings)
(defn solve
	"Given a puzzle, an initial population size, and specified numbers of
	 generations and phases, runs the genetic algorithm
	 until either a solution is found, in which case the vector of moves that
	 solved the chromosome is returned, or Nil if a solution is not found 
	 within the specified limits"
	([puzzle]
		(let [opt (optimal-settings puzzle)]
			(solve puzzle (opt 0) (opt 1) (opt 2))))
	([puzzle pop-size num-phases num-gens heuristic]
		(if (validate-params new-params)
		 	(binding [params (assoc params :heuristic heuristic)]
		 		(println "hey")
		 		(solve puzzle pop-size num-phases num-gens))
			(throw (Exception. "Invalid parameters."))))
	([puzzle pop-size num-phases num-gens]
		(loop [n num-phases
		   puz puzzle
		   solution []]
			(if (zero? n)
				nil
				(let [best-chrom (run-phase puzzle pop-size num-gens)
					  interp (interpret-chromosome best-chrom puzzle)
					  new-puzzle (interp 0)
					  new-solution (into [] (concat solution (interp 1)))]
					(if (solved? new-puzzle)
						new-solution
						(recur (- n 1) new-puzzle new-solution)))))))

; PRIVATE FUNCTIONS
(defn- optimal-settings
	"Give a puzzle, returns the default optimal settings for that puzzle size
	in a vector in the order population size, phases, number of generations"
	[puzzle]
	(let [mult (* (:rows puzzle) (:cols puzzle))]
		(cond
		 	(< mult 5) [10 1 10]
		 	(< mult 9) [100 2 100]
		 	:else [200 5 500])))

(defn- validate-params
	"Makes sure that the weights for the mutations/crossover and the
	fitness/cost each sum up to 1 and that the heuristic function is valid"
	[{h-w :h-weight c-w :c-weight cross-w :cross-weight mut-w :mut-weight heur :heuristic}]
	(and (= (+ h-w c-w) 1) (= (+ cross-w mut-w) 1) (:heuristic (meta heur))))

(defn- opposite-dir
	"Given a direction, returns the opposite direction 
	returns nil if an invalid direction keyword is passed in"
	 [dir]
	 (case dir
	 	:up :down
	 	:down :up
	 	:left :right
	 	:right :left
	 	nil))

(defn- gen-chromosome
	"Given a desired chromosome length, returns a list of genes where each
	 gene is a floating point number in (0, 1]"
	[length]
	(into [] (map (fn [x] (rand)) (range 0 length))))

(defn- gen-population
	"Given a pop-size and a chromosome size,
	returns a population (vector of chromosomes) of pop-size chromosomes each
	of length chrom-size"
	[pop-size chrom-size]
	(into [] (for [x (range pop-size)]
		(gen-chromosome chrom-size))))

(defn- mutate
	"Given a chromosome, replace a randomly chosen index with a new gene."
	[chromosome]
	(assoc chromosome (rand-int (+ (count chromosome) -1)) (rand)))

(defn- interpret-chromosome
	"Given a chromosome and a puzzle, returns a vector containing the final
	puzzle state and a vector of the directions the puzzle was slid"
	[chromosome puzzle]
	(loop [chrom chromosome
		   puz puzzle
		   prev-dir nil
		   dir-list []]
		(if (empty? chrom)
			[puz, dir-list]
			(let [undo-dir (opposite-dir prev-dir)
				  valid-dirs (into [] (filter #(not (= % undo-dir)) (valid-directions puz)))
				  next-dir (valid-dirs (int (* (chrom 0) (count valid-dirs))))
				  new-chrom (into [] (drop 1 chrom))]
				(recur new-chrom (slide puz next-dir) next-dir (conj dir-list next-dir))))))

(defn- fitness
	"Given a chromosome, determines the fitness,
	(right now, this is just the manhattan-distance of the final puzzle state 
	that corresponds to interpreting the chromosome as a list of moves relative
	to the puzzle)"
	[chromosome puzzle]
	(let [match-score (* (params :h-weight) ((params :heuristic) ((interpret-chromosome chromosome puzzle) 0)))
		  cost (* (params :c-weight) (count chromosome))]
		(+ match-score cost)))

(defn- rand-crossover
	"Given two chromosomes, picks a random index idx1 in chrom1
	and a random index idx2 in chromosome. Returns the chromosome that results 
	from taking the first idx1 genes from chrom1 and concat'ing on the the
	genes in chrom2 from idx2 until the end"
	[chrom1 chrom2]
	(let [idx1 (rand-int (count chrom1))
		  idx2 (rand-int (count chrom2))]
		(into [] (concat (take idx1 chrom1) (drop idx2 chrom2)))))

(defn- best
	"Given a population, returns the most fit chromosomes"
	[population puzzle]
	(first (sort-by #(fitness % puzzle) population)))

(defn- battle
	"Given two chromosomes and a puzzle, returns the one with the
	smaller (better) fitness relative to the puzzle"
	[chrom1 chrom2 puzzle]
	(if (> (fitness chrom1 puzzle) (fitness chrom2 puzzle))
		chrom2
		chrom1))

(defn- tournament
	"Given a population of size n and a puzzle, returns a new population of size
	n / 2 by randomly choosing pairs from the old population and keeping
	the more fit (lower fitness score) choice relative to the puzzle"
	[population puzzle]
	(loop [old-pop (shuffle population)
		   new-pop []]
		(let [pair (take 2 old-pop)
			  next-old-pop (drop 2 old-pop)]
			(if (< (count pair) 2)
				new-pop
				(let [winner (battle (first pair) (last pair) puzzle)]
				(recur next-old-pop (conj new-pop winner)))))))

(defn- perform-crossover
	"Given a population and a number of crossovers, returns num-cross
	number of crossed-over chromosome based on randomly chosen pairs"
	[p num-cross]
	(into [] (repeatedly num-cross #(rand-crossover (rand-nth p) (rand-nth p)))))


(defn- perform-mutation
	"Given a population and a number of mutations, returns num-mut 
	mutated chromsomes based on randomly chosen chromosomes in the population"
	[p num-mut]
	(into [] (repeatedly num-mut #(mutate (rand-nth p)))))

(defn- run-generation
	"Given a population, the fraction of crossovers to perform, the fraction of
	mutations to perform (NB: cross-chance mut-chance should sum to 1), and the
	puzzle currently being solved, using tournament selection, crossover and 
	mutation to return a new population"
	[population cross-chance mut-chance puzzle]
	(let [new-pop (tournament population puzzle)
		  new-pop-size (count new-pop)
		  num-cross (math/ceil (* cross-chance new-pop-size))
		  num-mut (math/ceil (* mut-chance new-pop-size))
		  crossovers (perform-crossover new-pop num-cross)
		  mutations (perform-mutation new-pop num-mut)]
		  (into [] (concat new-pop crossovers mutations))))

(defn- run-phase
	"Given a starting puzzle state, an initial population size and
	the number of generations to run, runs num-gens generations feeding the
	population returned from the previous generation into the next one and
	ultimately returning the most fit (lowest fitness score) chromosome
	from the final population"
	[{rows :rows :as puzzle} pop-size num-gens]
	(let [rows-sq (* rows rows)
		  chrom-length (int (/ (* rows-sq (+ rows-sq -1)) 2))
		  population (gen-population pop-size chrom-length)]
		(loop [p population
			   n num-gens]
			(if (zero? n)
				(best p puzzle)
				(let [new-pop (run-generation p (params :cross-weight) (params :mut-weight) puzzle)]
					(recur new-pop (+ n -1)))))))