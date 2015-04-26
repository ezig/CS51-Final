(ns npuzzles.genetic
	(:use [npuzzles.puzzle])
	(:use [clojure.set :only (intersection)]))

(defrecord Gene [puzzle prev-dir])

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
	"Given a direction, returns the opposite"
	 [dir]
	 (case dir
	 	:up :down
	 	:down :up
	 	:left :right
	 	:right :left
	 	nil))

(defn gen-chromosome
	"Given a starting puzzle state, make moves number of random (valid) moves
	to build a list of genes"
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

(defn interpret-chromsome
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
	(manhattan-distance ((interpret-chromsome chromosome puzzle) 0)))

(defn rand-crossover
	"Given two chromosomes, finds the points in the chromosomes where
	 the puzzle states are the same, picks a random point from this list,
	 and returns a new chromosome that takes the leading part of the first
	 and concats it with the trailing part of the second chromosome."
	[chrom1 chrom2]
	(let [idx1 (rand-int (count chrom1))
		  idx2 (rand-int (count chrom2))]
		(into [] (concat (take idx1 chrom1) (drop idx2 chrom2)))))

; (defn n-best
; 	"Given a population, returns the n most fit chromosomes"
; 	[population n]
; 	(take n (sort-by fitness population)))

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
	(loop [old-pop population
		   new-pop []]
		(let [pair (take 2 old-pop)
			  next-old-pop (drop 2 old-pop)]
			(if (< (count pair) 2)
				new-pop
				(let [winner (battle (first pair) (last pair) puzzle)]
				(recur next-old-pop (conj new-pop winner)))))))

; (defn run-generation
; 	"Given a population, the number of best to pick from the generation,
; 	the number of crossover chromosomes to generation, the mutation probability
; 	(int out of 100), the max size of the individual chromosomes returns a
; 	new population sorted by fitness"
; 	[population num-best num-cross mut-prob max-size]
; 	(let [best (n-best population num-best)
; 		  crossovers (repeatedly num-cross #(crossover (rand-nth best) (rand-nth best)))
; 		  new-pop (concat best crossovers)
; 		  mut-pop (map #(if (< (rand-int 100) mut-prob) (mutate % max-size) %) new-pop)]
; 		  (vec (sort-by fitness mut-pop))))

(defn run-phase
	"Given a starting puzzle state, generates a population of pop-size
	 chromosomes and then runs num-gens number of generations within the phase.
	 run-phase will return a solution as soon as it finds one, otherwise
	 return the best chromosome from the phase"
	[puzzle pop-size num-gens]
	(let [population (gen-population puzzle pop-size (:rows puzzle))
		  num-best (* 2 (:rows puzzle))
		  num-cross num-best
		  mut-prob 50
		  max-size (* 20 (:rows puzzle))]
		(loop [n num-gens
			   p population]
			(if (= n 0)
				(first p)
				(let [new-pop (run-generation p num-best num-cross mut-prob max-size)
					  best (first new-pop)]
					(if (solved? (:puzzle (last best)))
						(first new-pop)
						(recur (- n 1) new-pop)))))))