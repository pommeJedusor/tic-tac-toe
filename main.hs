import Data.Int
import Data.Bits
import Text.Printf

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
  | any (\move -> snd move == -1) moves = (fst $ head (filter (\move -> snd move == -1) moves), 1)
  | any (\move -> snd move == 0) moves  = head (filter (\move -> snd move == 0) moves)
  | otherwise                           = (fst $ head moves, -1)

minimax :: Int16 -> Int16 -> Int -> (Int, Int)
minimax p1Grid p2Grid move
  | isWinning p2Grid                          = (move, -1)
  | (p1Grid .|. p2Grid) `xor` gridFilter == 0 = (move, 0)
  | otherwise                                 = getBestMove (map (\x -> minimax p2Grid (makeMove p1Grid x) (if move==42 then x else move)) (getMoves p1Grid p2Grid))

moveToXY :: Int -> (Int, Int)
moveToXY move = (move `mod` 4 + 1, move `div` 4 + 1)

getCharBoard :: Int16 -> Int16 -> Int -> Char
getCharBoard board1 board2 pos
  | (pos+1) `mod` 4 == 0        = '\n'
  | shiftL 1 pos .&. board1 > 0 = '1'
  | shiftL 1 pos .&. board2 > 0 = '2'
  | otherwise                   = '0'

seeBoard p1Grid p2Grid = do
  putStrLn $ map (getCharBoard p1Grid p2Grid) [0..10]

main :: IO ()
main = do
    let p1Grid = 0b0000_0000_0000
    let p2Grid = 0b0000_0000_0000
    seeBoard p1Grid p2Grid
    let move0 = minimax p1Grid p2Grid 42
    printf "le coup joué est %d\n" $ fst move0
    seeBoard p2Grid . makeMove p1Grid $ fst move0