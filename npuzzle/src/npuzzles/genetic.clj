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
;
; A chromosome is a list of floating point numbers that must be interpreted
; in the context of a particular puzzle because the list of valid moves 
; changes relative to the state of the puzzle. Thus, given a particular
; state with an associated list of valid moves, each gene represents which
; of those moves to take. 

; (time (dotimes [n 10] (println (count (filter #(not (nil? %)) (map #(solve % 10 1 10) (filter #(solvable? %) (map #(gen-puzzle 2 2 %) (map #(into [] %) (permutations [1 2 3 0]))))))))))

(ns npuzzles.genetic
	(:use [npuzzles.puzzle])
	(:require [clojure.math.numeric-tower :as math])
    (:require [taoensso.timbre.profiling :as profiling
           :refer (pspy pspy* profile defnp p p*)]))

;PUBLIC FUNCTIONS

(declare run-phase)
(declare interpret-chromosome)
(defn solve
	"Given a puzzle, an initial population size, and specified numbers of
	 generations and phases, runs the genetic algorithm
	 until either a solution is found, in which case the vector of moves that
	 solved the chromosome is returned, or Nil if a solution is not found 
	 within the specified limits"
	[puzzle pop-size num-phases num-gens]
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
						(recur (- n 1) new-puzzle new-solution))))))

; PRIVATE FUNCTIONS
; TODO: PRIVATIZE THESE WHEN DONE TESTING
(defnp opposite-dir
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

(defn mutate
	"Given a chromosome, replace a randomly chosen index with a new gene."
	[chromosome]
	(assoc chromosome (rand-int (+ (count chromosome) -1)) (rand)))

(defnp interpret-chromosome
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

(defnp fitness
	"Given a chromosome, determines the fitness,
	(right now, this is just the manhattan-distance of the final puzzle state 
	that corresponds to interpreting the chromosome as a list of moves relative
	to the puzzle)"
	[chromosome puzzle]
	(let [match-score (* 0.9 (manhattan-distance ((interpret-chromosome chromosome puzzle) 0)))
		  cost (* 0.1 (count chromosome))]
		(+ match-score cost)))

(defn rand-crossover
	"Given two chromosomes, picks a random index idx1 in chrom1
	and a random index idx2 in chromosome. Returns the chromosome that results 
	from taking the first idx1 genes from chrom1 and concat'ing on the the
	genes in chrom2 from idx2 until the end"
	[chrom1 chrom2]
	(let [idx1 (rand-int (count chrom1))
		  idx2 (rand-int (count chrom2))]
		(into [] (concat (take idx1 chrom1) (drop idx2 chrom2)))))

(defn best
	"Given a population, returns the most fit chromosomes"
	[population puzzle]
	(first (sort-by #(fitness % puzzle) population)))

(defn battle
	"Given two chromosomes and a puzzle, returns the one with the
	smaller (better) fitness relative to the puzzle"
	[chrom1 chrom2 puzzle]
	(if (> (fitness chrom1 puzzle) (fitness chrom2 puzzle))
		chrom2
		chrom1))

(defn tournament
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

(defn perform-crossover
	"Given a population and a number of crossovers, returns num-cross
	number of crossed-over chromosome based on randomly chosen pairs"
	[p num-cross]
	(into [] (repeatedly num-cross #(rand-crossover (rand-nth p) (rand-nth p)))))


(defn perform-mutation
	"Given a population and a number of mutations, returns num-mut 
	mutated chromsomes based on randomly chosen chromosomes in the population"
	[p num-mut]
	(into [] (repeatedly num-mut #(mutate (rand-nth p)))))

(defn run-generation
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

(defn run-phase
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
				(recur (run-generation p 0.9 0.1 puzzle) (+ n -1))))))