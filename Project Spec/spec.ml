(* gives an abstract way of representing directions to move *)
type Direction = Left | Right | Up | Down

(* Class for representing tile puzzle *)
class Puzzle
	instance variables:

	(*dimension of the board*)
	val size : int
	(* containing all of the tiles in order. We think that
	 * using an int list will be easier to work with instead of an int list list*)
	val tiles : int list 
	(* keeps track of where the empty tile is *)
	val empty: int
	(* keeps track of the last move made *)
	val last_move: direction
	(* keeps track of what the solved state of the puzzle should be *)
	val solution: int list  

	public methods:

	(*takes in the size and returns a new puzzle*)
	initialize: int -> Puzzle 
	(* takes in a list of ints and creates puzzle with specified tile order
	 * will include check to see if created puzzle is solvable*)
	initialize: int list -> Puzzle 
	(* mixes up the tiles randomly until a solvable permutation is found*)
	shuffle: void -> void 
	(* slides the puzzle in the specified direction, if move is valid *)
	move: direction -> void
	(* returns true if current arrangement matches solution *)
	solved?:void -> bool 
	(* checks if a list of moves will solve the puzzle *)
	check_solution: direction list -> bool
	(* returns a list of valid moves from current arrangment *)
	valid_moves: void -> direction list
	(* uses the provided heuristic function to return a
	 fitness score representing how close the puzzle is to being solved *)
	fitness: (heuristic function) -> int 

	private methods:
	(* determines if current permutation can be solved *)
	solvable?: void -> bool 
	(* swaps tiles around (should only be called if direction is a valid move*)
	slide: direction -> void 
end

class GeneticSolver
	(* Abstract type to represent an individual solution candidate *)
	type chromosome : float list
	(* Abstract type to represent a collection of solution candidate *)
	type population : (float list) list

	instance variables:

	(* initial puzzle we are solving *)
	val puzzle: Puzzle 
 
	public methods:

	(* initializes a genetic solver with a given puzzle *)
	initialize: Puzzle -> GeneticSolver 
	(* Runs genetic algorithms and returns a list of directions 
	 * to solve if a solution was found, None otherwise. *)
	solve: void -> (direction list) option 

	private methods:
	(* takes in a population size and the length we want each chromosome to be
	 * and randomly creates a starting population *)
	generate_population : int -> int -> population 
	(* takes in two chromosomes and performs a crossover operation to
	 * produce a new one *)
	crossover : chromosome -> chromosome -> chromosome 
	(*  randomly alters a chromosome *)
	mutation : chromosome -> chromosome
	(* evaluates fitness of population, generates crossovers and mutations
	 * for next generation *)
	selection : population -> population
	(* contextualizes chromosome for specific puzzle state, apply move list 
	 * to puzzle, evaluate the fitness of the resulting partial solution *)
	fitness : chromosome -> int
	(*takes in 4 parameters: number of phases to run, number of generations in
	 * each phase, number of chromosomes in each population, and length of chromosome
	 *. Will keep running phases until a solution is found or the phase count reaches 0 *)
	run_genetic_alg : int -> int -> int -> int -> (direction list) option 
	(*  takes in the number of generations to run, returns a direction list if it finds
	 * a solution, None otherwise *)
	run_phase : int -> (direction list) option

end

(* A* Implementation: *)
class AStarSolver

	Instance Variables:
	(* space containing the list of puzzles that are within one move of the
	 * current state of the puzzle (discluding puzzle that would bring it back
	 * to the previous state *)
	mutable open_space: Puzzle list
	(* moves made up to current state *)
    mutable moves : Direction list
    (* space containing the list of puzzles that correspond to the best choice
    * puzzle at each step in the solving process  (based on heuristic score) *)
    mutable closed_space: Puzzle list
          
	Public Methods
	method run_solver: puzzle -> Direction list 

end 
