; Puzzle record has integer number of rows, columsns
; and an row * column length vector of tiles [0, row * column)
(defrecord Puzzle [rows cols tiles])

(defn gen-puzzle
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

;TODO: NOT SURE IF THIS IS CORRECT FOR PUZZLES WHERE rows != cols
(defn solvable?
	"Returns true if a given puzzle is solvable, false otherwise"
	[{rows :rows cols :cols :as puzzle}]
	(or
		(and (odd? (* rows cols)) (even? (inversions puzzle)))
		(= (even? (inversions puzzle)) (odd? (row-of-tile puzzle 0)))))

(defn row-of-tile
	"Returns the row that a given tile is on"
	[{rows :rows tiles :tiles} tile]
	(quot (.indexOf tiles tile) rows))

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
	"Given a vector and two indices, swaps the values"
	[v i1 i2]
	(assoc v i1 (v i2), i2 (v i1)))