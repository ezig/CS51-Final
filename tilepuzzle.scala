class TilePuzzle(n : Int)
{
    val size : Int = n
    private var empty : Int = size * size - 1
    private var tiles : Vector[Int] =
        (1 to (size * size - 1)).toVector :+ 0

    def shuffle () = 
    {
        tiles = util.Random.shuffle(tiles)
        empty = tiles.indexOf(0)
    }

    def validMoves () : Vector[Char] = 
    {
        var moves : Vector[Char] = Vector()
        
        if (empty / size != 0)
        {
            moves = moves :+ 'U'
        }
        if (empty / size != size - 1)
        {
            moves = moves :+ 'D'
        }
        if (empty % size != 0)
        {
            moves = moves :+ 'L'
        }
        if (empty % size != size -1)
        {
            moves = moves :+ 'R'
        }

        moves
    }
}

val puzzle = new TilePuzzle(2)
println(puzzle.validMoves)