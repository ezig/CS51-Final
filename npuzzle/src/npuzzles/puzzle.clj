; Puzzle record has integer number of rows, columsns
; and an row * column length vector of tiles [0, row * column)
; Puzzle is solved if tiles are in increasing order and zero is in bottom right
(defrecord Puzzle [rows cols tiles])

(defn random-puzzle
	"Given a number of rows and columns, creates random puzzle"
	[r c]
	{:rows r, :cols c, :tiles (shuffle (range (* r c)))})

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
     ([rows cols tiles] (Puzzle. rows cols tiles)))

(defn to-string
	"Given a puzzle, returns a string representation"
	[{rows :rows tiles :tiles}]
	(loop [string (str (first tiles))
		   lst (rest tiles)]
		   (if (empty? lst)
		   	   string
		   	   (let [hd (first lst) tl (rest lst)]
				   (if (= (mod (count lst) rows) 0)
				   	   (recur (str string "\\n" hd) tl)
				   	   (recur (str string hd) tl))))))

(defn solved?
	"Given a puzzle, returns true if it is solved"
	[{rows :rows cols :cols tiles :tiles}]
	(= tiles (concat (range 1 (* rows cols)) (list 0))))

(defn swap
	"Given a vector and two indices, swaps the values at those indices"
	[v i1 i2]
	(assoc v i1 (v i2), i2 (v i1)))

(defn find-tile
	"Returns the index of a tile in a puzzle"
	[{tiles :tiles} tile]
	(.indexOf tiles tile))

(defn row-of-tile
	"Returns the row that a given tile is on"
	[{rows :rows :as puzzle} tile]
	(quot (find-tile puzzle tile) rows))

(defn col-of-tile
	"Returns the col that a given tile is on"
	[{cols :cols :as puzzle} tile]
	(mod (find-tile puzzle tile) cols))

(defn valid-directions
	"Given a puzzle, returns a hash map of directions and associated booleans"
	[{rows :rows cols :cols :as puzzle}] 
	(let [zeroRow (row-of-tile puzzle 0)
		  zeroCol (col-of-tile puzzle 0)]
		  {:up (not (= zeroRow 0)), :down (not (= zeroRow (- rows 1))),
		   :left (not (= zeroCol 0)), :right (not (= zeroCol (- cols 1)))}))

(defn slide
	"Given a direction, moves puzzle in that direction if it is a valid move"
	[{cols :cols rows :rows tiles :tiles :as puzzle} direction]
	(if (get (valid-directions puzzle) direction)
		(let [emptyP (find-tile puzzle 0)
			newP (case direction
				:up (- emptyP cols)
				:down (+ emptyP cols)
				:left (- emptyP 1)
				:right (+ emptyP 1))]
			{:rows rows :cols cols :tiles (swap tiles emptyP newP)})
		puzzle))

(defn inversions
	"Given a puzzle, returns the number of inversions"
	[{tiles :tiles}]
	(loop [inv 0
		   lst (remove #(= % 0) tiles)]
		(if (empty? lst)
			inv
			(let [hd (first lst) tl (rest lst)]
				(recur (+ inv (count (filter #(< % hd) tl))) tl)))))

;TODO: NOT SURE IF THIS IS CORRECT FOR PUZZLES WHERE rows != cols
(defn solvable?
	"Returns true if a given puzzle is solvable, false otherwise"
	[{rows :rows cols :cols :as puzzle}]
	(let [is_odd (odd? (* rows cols))]
		(or
	  		(and is_odd (even? (inversions puzzle)))
	  		(and (not is_odd) (= (even? (inversions puzzle)) (odd? (row-of-tile puzzle 0)))))))