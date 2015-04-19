(ns npuzzles.geneticsolver
	(:use [npuzzles.puzzle]))

(defrecord Gene [puzzle prev-dir])

;PUBLIC FUNCTIONS

(defn solve
	"Given a puzzle, runs the genetic algorithm with specified number of
	 generations and phases until either a solution is found,
	 in which case the chromosome that solves the puzzle is returned,
	 or Nil if a solution is not found within the specified limits"
	[puzzle generations phases])

; PRIVATE FUNCTIONS
; TODO: PRIVATIZE THESE WHEN DONE TESTING
(defn random-dir
	"Given a puzzle, returns a random valid direction that is NOT dir
	Based on the assumption that the puzzle has 2 valid moves in every state"
 	[puzzle dir]
 	(let [valid (filter #(not (= % dir)) (valid-directions puzzle))]
 		(rand-nth valid)))

(defn opposite-dir
	"Given a direction, returns the opposite"
	 [dir]
	 (case dir
	 	:up :down
	 	:down :up
	 	:left :right
	 	:right :left
	 	nil))

(defn fitness
	"Given a chromosome, determines the fitness
	(right now, this is just the manhattan-distance of the last gene)"
	[chromosome]
	(manhattan-distance (:puzzle (last chromosome))))

(defn next-gene
	"Given a gene, returns a gene that represents a possible next state
	The new gene will be a valid state within one move of the original gene
	and will NOT undo the previous move"
	[{puzzle :puzzle prev-dir :prev-dir}]
	(let [next-dir (random-dir puzzle (opposite-dir prev-dir))]
		{:puzzle (slide puzzle next-dir) :prev-dir next-dir}))

(defn run-phase
	"Given a starting puzzle state, generates a population of pop-size
	 chromosomes and then runs num-gens number of generations within the phase.
	 run-phase will return a solution as soon as it finds one, otherwise
	 return the best chromosome from the phase"
	[puzzle pop-size num-gens])

(defn generate-population
	"Given an initial pop-size, creates list random chromosomes"
	[pop-size])

(defn generate-chromosome
	"Given a starting puzzle state, make moves number of random (valid) moves
	to build a list of genes"
	[puzzle moves]
	(loop [last-gene {:puzzle puzzle, :prev-dir nil}
		   chromosome [last-gene]
		   n moves]
		(if (zero? n)
			chromosome
			(let [new-gene (next-gene last-gene)]
				(recur new-gene (conj chromosome new-gene) (- n 1))))))

(defn mutate
	"Given a chromosome, extend the chromosome create another valid puzzle
	by moving the current puzzle. If this results in a chromosome beyond
	max-size, the chromosome will be truncated randomly"
	[chromosome max-size]
	(let [new-chromosome (conj chromosome (next-gene (last chromosome)))]
		(if (> (count new-chromosome) max-size)
			(drop (+ 1 (rand-int max-size)) new-chromosome)
			new-chromosome)))

(defn crossover
	"Given two chromosomes, finds the points in the chromosomes where
	 the puzzle states are the same, picks a random point from this list,
	 and returns a new chromosome that takes the leading part of the first
	 and concats it with the trailing part of the second chromosome."
	[chromosome1 chromosome2])