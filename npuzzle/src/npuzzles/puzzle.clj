(defn swap
	"Given a vector and two indices, swaps the values"
	[v i1 i2]
	(assoc v i1 (v i2), i2 (v i1)))