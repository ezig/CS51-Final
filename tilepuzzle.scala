sealed abstract class Direction
case object Left extends Direction
case object Right extends Direction
case object Up extends Direction
case object Down extends Direction

class TilePuzzle(n : Int)
{
    val size : Int = n
    private var empty : Int = size * size - 1
    private var tiles : Vector[Int] =
        (1 to (size * size - 1)).toVector :+ 0

    def shuffle ()
    {
        tiles = util.Random.shuffle(tiles)
        empty = tiles.indexOf(0)
    }

    def validMoves () : Vector[Direction] = 
    {
        var moves : Vector[Direction] = Vector()
        
        if (empty / size != 0)
        {
            moves = moves :+ Up
        }
        if (empty / size != size - 1)
        {
            moves = moves :+ Down
        }
        if (empty % size != 0)
        {
            moves = moves :+ Left
        }
        if (empty % size != size -1)
        {
            moves = moves :+ Right
        }

        moves
    }

    private def slide (dir : Direction)
    {
        var newEmpty : Int = empty

        dir match 
        {
            case Left => newEmpty -= 1
            case Right => newEmpty += 1
            case Down => newEmpty += size
            case Up => newEmpty -= size
        }

        swapTiles(empty, newEmpty)
        empty = newEmpty
    }

    private def swapTiles(x : Int, y : Int)
    {
        val xVal = tiles(x)
        val yVal = tiles(y)
        tiles = tiles.updated(x, yVal).updated(y, xVal)
    }
}