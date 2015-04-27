(ns npuzzles.puzzle)

; Puzzle record has integer number of rows, columsns
; and an row * column length vector of tiles [0, row * column)
; Puzzle is solved if tiles are in increasing order and zero is in bottom right
(defrecord Puzzle [rows cols tiles])

;PUBLIC FUNCTIONS

(declare random-puzzle)
(declare solvable?)
(defn random-solvable-puzzle
	"Given rows and columns, generates a random puzzle that is solvable"
	[rows cols]
	(loop [puzzle (random-puzzle rows cols)]
		(if (solvable? puzzle)
			puzzle
			(recur (random-puzzle rows cols)))))
(defn gen-puzzle
	"Given one dimension, generates random solvable square puzzles
	 Given two dimensions, generates a random solvable puzzle.
	 Given dimensions and tiles, generates puzzle with given tiles
	  (intended only for testing)"
	 ([rows] (random-solvable-puzzle rows rows))
     ([rows cols] (random-solvable-puzzle rows cols))
     ([rows cols tiles] {:rows rows, :cols cols, :tiles tiles}))

(defn to-string
	"Given a puzzle, returns a string representation"
	[{cols :cols tiles :tiles}]
	(loop [string (str (first tiles))
		   lst (rest tiles)]
		   (if (empty? lst)
		   	   string
		   	   (let [hd (first lst) tl (rest lst)]
				   (if (= (mod (count lst) cols) 0)
				   	   (recur (str string "\n" hd) tl)
				   	   (recur (str string hd) tl))))))

(declare row-of-tile)
(declare col-of-tile)
(defn valid-directions
	"Given a puzzle, returns a list of direction keywords of valid moves"
	[{rows :rows cols :cols :as puzzle}] 
	(let [zeroRow (row-of-tile puzzle 0)
		  zeroCol (col-of-tile puzzle 0)
		  moves {:up (not (= zeroRow (- rows 1))), :down (not (= zeroRow 0)),
		   :left (not (= zeroCol (- cols 1))) , :right (not (= zeroCol 0))}]
		(vec (keys (into {} (filter #(val %) moves))))))

(declare swap)
(declare find-tile)
(defn slide
	"Given a direction, moves puzzle in that direction if it is a valid move"
	[{cols :cols rows :rows tiles :tiles :as puzzle} direction]
	(if (not (nil? (some #{direction} (valid-directions puzzle))))
		(let [emptyP (find-tile puzzle 0)
			newP (case direction
				:up (+ emptyP cols)
				:down (- emptyP cols)
				:left (+ emptyP 1)
				:right (- emptyP 1))]
			{:rows rows :cols cols :tiles (swap tiles emptyP newP)})
		puzzle))

(defn solved?
	"Given a puzzle, returns true if it is solved"
	[{rows :rows cols :cols tiles :tiles}]
	(= tiles (concat (range 1 (* rows cols)) (list 0))))

(declare abs)
(defn manhattan-distance
	"Given a Puzzle, calculates its fitness using the Manhattan Distance 
	 heuristic function"
	[{cols :cols rows :rows tiles :tiles :as puzzle}]
	(loop [d 0 
		   lst tiles]
	 	(if (empty? lst)
	 	    d
	 	    (let [hd (first lst) tl (rest lst)
 	     		  final_index (mod (- hd 1) (* rows cols))
	 	     	  final_row (quot final_index cols)
	 	     	  final_col (mod final_index cols)
				  column (col-of-tile puzzle hd)
				  row (row-of-tile puzzle hd)]
            	(recur (+ d (+ (abs (- row final_row)) (abs (- column final_col))))
            		tl)))))

(defn dir-between
	"Given two puzzles, determines the direction to slide from puzzle1
	to get puzzle2. ASSUMES PUZZLES HAVE SAME DIMENSION"
	[puzzle1 puzzle2]
	(let [diff (- (find-tile puzzle2 0) (find-tile puzzle1 0))]
		(cond 
			(= -1 diff) :right
			(= 1 diff) :left
			(= (:cols puzzle1) diff) :up
			(= (- (:cols puzzle1)) diff) :down
			:else nil)))

(defn check-solution
	"Given a puzzle and a vector of directions representing a solution,
	slides the puzzle by the sequence of directions and returns true if the
	final puzzle is solved, false otherwise"
	[puzzle dirs]
	(loop [puz puzzle
		   dir-list dirs]
		(if (empty? dir-list)
			(solved? puz)
			(recur (slide puz (first dir-list)) (rest dir-list)))))



;PRIVATE FUNCTIONS
(defn- abs [n] (max n (- n)))

(defn- swap
	"Given a vector and two indices, swaps the values at those indices"
	[v i1 i2]
	(assoc v i1 (v i2), i2 (v i1)))

(defn- find-tile
	"Returns the index of a tile in a puzzle"
	[{tiles :tiles} tile]
	(.indexOf tiles tile))

(defn- row-of-tile
	"Returns the row that a given tile is on"
	[{cols :cols :as puzzle} tile]
	(quot (find-tile puzzle tile) cols))

(defn- col-of-tile
	"Returns the col that a given tile is on"
	[{cols :cols :as puzzle} tile]
	(mod (find-tile puzzle tile) cols))

(defn- inversions
	"Given a puzzle, returns the number of inversions"
	[{tiles :tiles}]
	(loop [inv 0
		   lst (remove #(= % 0) tiles)]
		(if (empty? lst)
			inv
			(let [hd (first lst) tl (rest lst)]
				(recur (+ inv (count (filter #(< % hd) tl))) tl)))))
; MADE THIS PUBLIC FOR TESTING PURPOSES
(defn solvable?
	"Returns true if a given puzzle is solvable, false otherwise"
	[{rows :rows cols :cols :as puzzle}]
	(let [is_odd (odd? (* rows cols))]
		(or
	  		(and (odd? cols) (even? (inversions puzzle)))
	  		(and 
	  			(even? cols)
	  			(even? rows)
	  			(odd? (+ (row-of-tile puzzle 0) (inversions puzzle))))
	  		(and 
	  			(even? cols)
	  			(odd? rows)
	  			(even? (+ (row-of-tile puzzle 0) (inversions puzzle)))))))

(defn- random-puzzle
	"Given a number of rows and columns, creates random puzzle
	MAY NOT BE SOLVABLE. Unsolvable puzzles should never be publically available"
	[r c]
	{:rows r, :cols c, :tiles (shuffle (range (* r c)))})