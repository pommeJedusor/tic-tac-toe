import Data.Int
import Data.Bits

-- constants
gridFilter :: Int16
gridFilter = 0b1110_1110_1110

firstDiagonal :: Int16
firstDiagonal = 0b1000_0100_0010

secondDiagonal :: Int16
secondDiagonal = 0b0010_0100_1000

-- functions
getMoves :: Int16 -> Int16 -> Int16
getMoves p1Grid p2Grid = (p1Grid .|. p2Grid) `xor` gridFilter

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

main :: IO ()
main = do
    let p1Grid = 0b0
    let p2Grid = 0b0
    print $ isWinning gridFilter