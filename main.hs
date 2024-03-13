import Data.Int
import Data.Bits

-- constants
gridFilter :: Int16
gridFilter = 0b0111_0111_0111

firstDiagonal :: Int16
firstDiagonal = 0b0100_0010_0001

secondDiagonal :: Int16
secondDiagonal = 0b0001_0010_0100

-- functions
getBinMoves :: Int16 -> Int16 -> Int16
getBinMoves p1Grid p2Grid = (p1Grid .|. p2Grid) `xor` gridFilter

getMoves :: Int16 -> Int16 -> [Int]
getMoves p1Grid p2Grid = filter (\x -> getBinMoves p1Grid p2Grid .&. shiftL 1 x > 0) [0..10]

makeMove :: Int16 -> Int -> Int16
makeMove grid move = grid .|. shiftL 1 move

horizontalWin :: Int16 -> Bool
horizontalWin grid = (grid .&. shiftL grid 1 .&. shiftL grid 2) > 0

verticalWin :: Int16 -> Bool
verticalWin grid = (grid .&. shiftL grid 4 .&. shiftL grid 8) > 0

diagonalWin :: Int16 -> Bool
diagonalWin grid = grid .&. firstDiagonal == firstDiagonal || grid .&. secondDiagonal == secondDiagonal

isWinning :: Int16 -> Bool
isWinning grid = horizontalWin grid || verticalWin grid || diagonalWin grid

getBestMove :: [(Int, Int)] -> (Int, Int)
getBestMove moves
  | any (\move -> snd move == -1) moves = head (filter (\move -> snd move == -1) moves)
  | any (\move -> snd move == 0) moves  = head (filter (\move -> snd move == 0) moves)
  | otherwise                           = head moves

minimax :: Int16 -> Int16 -> (Int, Int)
minimax p1Grid p2Grid
  | isWinning p2Grid                          = (11, -1)
  | (p1Grid .|. p2Grid) `xor` gridFilter == 0 = (11, 0)
  | otherwise                                 = getBestMove $ getMoves p1Grid p2Grid

main :: IO ()
main = do
    let p1Grid = 0b0
    let p2Grid = 0b0
    print $ getMoves p1Grid p2Grid