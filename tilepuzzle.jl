module TilePuzzle
  using Base.Test: @test

  type Board
    tiles::Array{Int,1}
    empty::Int
  end

  function newBoard(n::Int)
    tiles = [i for i=1:(n^2-1)]
    push!(tiles,0)
    return Board(tiles, n^2)
  end

  function shuffleBoard(b::Board)
    newTiles = shuffle(b.tiles)
    newEmpty = findfirst(newTiles, 0)
    return Board(newTiles, newEmpty)
  end

  function runTests()
    b = newBoard(2)
    @test(b.tiles == [1,2,3,0])
    @test(b.empty == 4)
  end
end

TilePuzzle.runTests()
