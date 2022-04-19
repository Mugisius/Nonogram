module Types where


-- Path to file with answer 
answerPath :: FilePath
answerPath = "answer.txt"

gridSize :: Int
gridSize = 40

spaceSize :: Int
spaceSize = 5

barrierSize :: Int
barrierSize = 7

renderShift :: Int
renderShift = 5

-- Current state of Board
data Board = Board
  { 
-- Size of board
    sizeX :: Int
  , sizeY :: Int
-- Hints 
  , hintsX :: Hints
  , hintsY :: Hints
-- State of cells that player has chosen
  , displayCells :: [[Cell]]
-- State of cells that player must choose to solve the puzzle
  , answerCells :: [[Cell]]
  }

-- Current game state (Playing or already Win)
data GameState = Playing Board Float Float| Win

-- State of cell type
data Cell = White | Black deriving Eq

-- Hints type
type Hints = [[Int]]

-- Error type
data Error = Error String

