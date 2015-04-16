; Puzzle record has integer number of rows, columsns
; and an row * column length vector of tiles [0, row * column)
(defrecord Puzzle [rows cols tiles])

(defn gen-puzzle
	"Given two dimensions, generates a random solvable puzzle.
	 Given dimensions and tiles, generates puzzle with given tiles
	  (intended only for testing)"
     ([rows cols] (random-solvable-puzzle rows cols))
     ([rows cols tiles] (Puzzle. rows cols tiles)))

(defn random-solvable-puzzle
	"Given rows and columns, generates a random puzzle that is solvable"
	[rows cols]
	(loop [puzzle (random-puzzle rows cols)]
		(if (solvable? puzzle)
			puzzle
			(recur (random-puzzle rows cols)))))

(defn random-puzzle
	"Given a number of rows and columns, creates random puzzle"
	[r c]
	{:rows r, :cols c, :tiles (shuffle (range (* r c)))})

;TODO: Probably a cleaner way to write this
(defn valid-directions
	"Given a puzzle, returns a list of valid directions"
	[{rows :rows cols :cols :as puzzle}]
	(let [zeroRow (row-of-tile puzzle 0)
		  zeroCol (col-of-tile puzzle 0)
		  valid [(= zeroRow 0) (= zeroRow (- rows 1))
		       (= zeroCol 0) (= zeroCol (- cols 1))]]
		(remove nil? (map #(when-not %1 %2) valid [:up :down :left :right]))))

(defn slide
	"Given a direction, moves puzzle in that direction if it is a valid move"
	[{cols :cols rows :rows tiles :tiles :as puzzle} direction]
	(if (some #{direction} (valid-directions puzzle))
		(let [emptyP (find-tile puzzle 0)
			newP (case direction
				:up (- emptyP cols)
				:down (+ emptyP cols)
				:left (- emptyP 1)
				:right (+ emptyP 1))]
				
			{:rows rows :cols cols :tiles (swap tiles emptyP newP)})
		puzzle))

;TODO: NOT SURE IF THIS IS CORRECT FOR PUZZLES WHERE rows != cols
(defn solvable?
	"Returns true if a given puzzle is solvable, false otherwise"
	[{rows :rows cols :cols :as puzzle}]
	(or
		(and (odd? (* rows cols)) (even? (inversions puzzle)))
		(= (even? (inversions puzzle)) (odd? (row-of-tile puzzle 0)))))

(defn find-tile
	"Returns the index of a tile in a puzzle"
	[{tiles :tiles} tile]
	(.indexOf tiles tile))

(defn row-of-tile
	"Returns the row that a given tile is on"
	[{rows :rows tiles :tiles} tile]
	(quot (.indexOf tiles tile) rows))

(defn col-of-tile
	"Returns the col that a given tile is on"
	[{cols :rows tiles :tiles} tile]
	(mod (.indexOf tiles tile) cols))

(defn inversions
	"Given a puzzle, returns the number of inversions"
	[{tiles :tiles}]
	(loop [inv 0
		   lst (remove #(= % 0) tiles)]
		(if (empty? lst)
			inv
			(let [hd (first lst) tl (rest lst)]
				(recur (+ inv (count (filter #(< % hd) tl))) tl)))))

(defn swap
	"Given a vector and two indices, swaps the values at those indices"
	[v i1 i2]
	(assoc v i1 (v i2), i2 (v i1)))