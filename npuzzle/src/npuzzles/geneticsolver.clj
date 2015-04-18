(ns npuzzles.geneticsolver
	(:require [npuzzles.puzzle :as puzzle]))

;PUBLIC FUNCTIONS

(defn solve
	"Given a puzzle, runs the genetic algorithm with specified number of
	 generations and phases until either a solution is found,
	 in which case the chromosome that solves the puzzle is returned,
	 or Nil if a solution is not found within the specified limits"
	[puzzle generations phases])

;PRIVATE FUNCTIONS

(defn- run-phase
	"Given a starting puzzle state, generates a population of pop-size
	 chromosomes and then runs num-gens number of generations within the phase.
	 run-phase will return a solution as soon as it finds one, otherwise
	 return the best chromosome from the phase"
	[puzzle pop-size num-gens])

(defn- generate-population
	"Given an initial pop-size, creates list random chromosomes"
	[pop-size])

(defn- generate-chromosome
	"Given a starting puzzle state, make moves number of random (valid) moves
	to build a list of puzzle states to form a chromosome."
	[puzzle moves])

(defn- random-dir
	"Given a puzzle, returns a random valid direction that is NOT dir"
 	[puzzle dir])

(defn- opposite-dir
	"Given a direction, returns the opposite"
	 [dir])

(defn- mutation
	"Given a chromosome, extend the chromosome create an- other valid puzzle
	by moving the current puzzle. If this results in a chromosome beyond
	max-size, the chromosome will be truncated randomly"
	[chromosome max-size])

(defn- crossover
	"Given two chromosomes, finds the points in the chromosomes where
	 the puzzle states are the same, picks a random point from this list,
	 and returns a new chromosome that takes the leading part of the first
	 and concats it with the trailing part of the second chromosome."
	[chromosome1 chromosome2])