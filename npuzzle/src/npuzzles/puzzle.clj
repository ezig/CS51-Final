; Puzzle record has integer number of rows, columsns
; and an row * column length vector of tiles [0, row * column)
(defrecord Puzzle [rows columns tiles])

(defn random-puzzle
	"Given a number of rows and columns, creates random puzzle"
	[r c]
	{:rows r, :cols c, :tiles (shuffle (range (* r c)))})

(defn inversions
	"Given a puzzle, returns the number of invertions"
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