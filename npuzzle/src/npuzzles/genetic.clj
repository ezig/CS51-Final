(ns npuzzles.genetic
	(:use [npuzzles.puzzle])
	(:require [clojure.math.numeric-tower :as math]))

;PUBLIC FUNCTIONS

(declare run-phase)
(defn solve
	"Given a puzzle, runs the genetic algorithm with specified number of
	 generations and phases until either a solution is found,
	 in which case the chromosome that solves the puzzle is returned,
	 or Nil if a solution is not found within the specified limits"
	[puzzle phases generations]
	(loop [n phases
		   puz puzzle
		   solution []]
			(if (= n 0)
				nil
				(let [best-chrom (run-phase puzzle (:rows puz) generations)
					  best-gene (last best-chrom)
					  new-solution (concat solution (drop 1 best-chrom))]
					(if (solved? (:puzzle best-gene))
						new-solution
						(recur (- n 1) (:puzzle best-gene) new-solution))))))

; PRIVATE FUNCTIONS
; TODO: PRIVATIZE THESE WHEN DONE TESTING
(defn opposite-dir
	"Given a direction, returns the opposite direction 
	returns nil if an invalid direction keyword is passed in"
	 [dir]
	 (case dir
	 	:up :down
	 	:down :up
	 	:left :right
	 	:right :left
	 	nil))

(defn gen-chromosome
	"Given a desired chromosome length, returns a list of genes where each
	 gene is a floating point number in (0, 1]"
	[length]
	(into [] (map (fn [x] (rand)) (range 0 length))))

(defn gen-population
	"Given an initial puzzle and a pop-size and chromosome size, creates
	list of random chromosomes"
	[pop-size chrom-size]
	(into [] (for [x (range pop-size)]
		(gen-chromosome chrom-size))))

(defn mutate
	"Given a chromosome, extend the chromosome create another valid puzzle
	by moving the current puzzle. If this results in a chromosome beyond
	max-size, the chromosome will be truncated randomly"
	[chromosome]
	(assoc chromosome (rand-int (+ (count chromosome) -1)) (rand)))

(defn interpret-chromosome
	"Given a chromosome and a puzzle, returns a the final puzzle
	and a vector of directions"
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

(defn fitness
	"Given a chromosome, determines the fitness
	(right now, this is just the manhattan-distance of the last gene)"
	[chromosome puzzle]
	(manhattan-distance ((interpret-chromosome chromosome puzzle) 0)))

(defn rand-crossover
	"Given two chromosomes, finds the points in the chromosomes where
	 the puzzle states are the same, picks a random point from this list,
	 and returns a new chromosome that takes the leading part of the first
	 and concats it with the trailing part of the second chromosome."
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
	"Given a population, the number of best to pick from the generation,
	the number of crossover chromosomes to generation, the mutation probability
	(int out of 100), the max size of the individual chromosomes returns a
	new population sorted by fitness"
	[population cross-chance mut-chance puzzle]
	(let [new-pop (tournament population puzzle)
		  new-pop-size (count new-pop)
		  num-cross (math/ceil (* cross-chance new-pop-size))
		  num-mut (math/ceil (* mut-chance new-pop-size))
		  crossovers (perform-crossover new-pop num-cross)
		  mutations (perform-mutation new-pop num-mut)]
		  (into [] (concat new-pop crossovers mutations))))

(defn run-phase
	"Given a starting puzzle state, generates a population of pop-size
	 chromosomes and then runs num-gens number of generations within the phase.
	 run-phase will return a solution as soon as it finds one, otherwise
	 return the best chromosome from the phase"
	[{rows :rows :as puzzle} pop-size num-gens]
	(let [chrom-length (int (/ (* (* rows rows) (+ (* rows rows) -1)) 2))
		  population (gen-population pop-size chrom-length)]
		(loop [p population
			   n num-gens]
			(if (zero? n)
				(best p puzzle)
				(recur (run-generation p 0.9 0.1 puzzle) (+ n -1))))))