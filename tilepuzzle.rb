class Direction
  Left = 0
  Right = 1
  Up = 2
  Down = 3
end

class TilePuzzle
    attr_reader :size, :tiles, :empty

    def initialize(n)
        @size = n
        @tiles =* (1..(n ** 2 -1))
        @tiles.push(0)
        @empty = n * n - 1
    end

    def shuffle()
        @tiles.shuffle!
        @empty = @tiles.index(0)
    end

    def slide(dir)
        newEmpty = empty

        case dir
        when Direction::Left
            newEmpty -= 1
        when Direction::Right
            newEmpty += 1
        when Direction::Up
            newEmpty -= size
        when Direction::Down
            newEmpty += @size
        end

        self.swapTiles(empty, newEmpty)
        @empty = newEmpty
    end

    def swapTiles(x, y)
        @tiles[x], @tiles[y] = @tiles[y], @tiles[x]
    end
end