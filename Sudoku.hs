{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Array ( Array
             , array
             , elems
             , (!))
import Data.Maybe (isJust, fromJust)

newtype Cell = Cell {values :: [Int]} deriving (Eq)

smallshow :: Cell -> String
smallshow (Cell [x]) = show x
smallshow _ = "-"

instance Show Cell where
  show cell = "(" ++ concatMap show (values cell) ++  ")"

filled :: Cell -> Bool
filled c = isJust (value c)

value :: Monad m => Cell -> m Int
value (Cell [x]) = return x
value _          = fail "Cell does not have a definite value"

newtype Board = Board { contents :: (Array (Int,Int) Cell) } deriving (Eq)

instance Show Board where
  show (Board a) = unlines [concat [smallshow (a ! (i,j)) | i <- [0..8]] | j <- [0..8]]

solved :: Board -> Bool
solved board = and $ elems $ fmap filled $ contents board

emptyCell :: Cell
emptyCell = Cell [1..9]

strToCell :: String -> Cell
strToCell "0" = emptyCell
strToCell n = Cell [read n]

strsToBoard :: [String] -> Board
strsToBoard strings = unrows cells
  where cells = [[strToCell [c] | c <- line] | line <- strings]

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards strings = b:bs
  where
    (bLines, bsLines) = splitAt 10 strings
    b = strsToBoard (drop 1 bLines)
    bs = parseBoards bsLines

rows, columns, squares :: Board -> [[Cell]]
unrows, uncolumns, unsquares :: [[Cell]] -> Board

rows (Board a) = [[a ! (i,j) | i <- [0..8]] | j <- [0..8]]

unrows cells = Board $ array ((0,0), (8,8)) alist
  where alist = [((i,j),cells !! j !! i) | i <- [0..8], j <- [0..8]]

columns (Board a) = [[a ! (j,i) | i <- [0..8]] | j <- [0..8]]

uncolumns cells = Board $ array ((0,0), (8,8)) alist
  where alist = [((i,j),cells !! i !! j) | i <- [0..8], j <- [0..8]]

sections :: [[Int]]
sections = [[0..2],[3..5],[6..8]]

squares (Board a) = [[a ! (j,i) | i <- is, j <- js] | is <- sections, js <- sections]

unsquares squareList = Board $ array ((0,0), (8,8)) alist
  where
    alist = [(squareCoords !! i !! j, squareList !! i !! j) | i <- [0..8], j <- [0..8]]
    squareCoords = [[(j,i) | i<-is, j<-js] | is <- sections, js <- sections]
  
-- | takes a row/column/square and returns a list of all the numbers that are already taken
taken :: [Cell] -> [Int]
taken row = do
  cell <- row
  n <- value cell
  return n

-- | Do as much solving as we can on a given row (or column or square)
solveRow :: [Cell] -> [Cell]
solveRow row = map removeTaken row
  where
    removeTaken c@(Cell [_]) = c
    removeTaken cell = Cell $ filter (`notElem` takens) (values cell)
    takens = taken row

solveRows :: [[Cell]] -> [[Cell]]
solveRows = map solveRow

solvePass :: Board -> Board
solvePass = (unrows . solveRows . rows) . (uncolumns . solveRows . columns) . (unsquares . solveRows . squares)

solve :: Board -> Board
solve board | solved board = board
            | otherwise    = solve (solvePass board)

verboseShow :: Board -> String
verboseShow board = show (rows board) ++ "\n\n"

signature :: Monad m => Board -> m Int
signature board = do
  let arr = contents board
  a <- value (arr ! (0,0))
  b <- value (arr ! (1,0))
  c <- value (arr ! (2,0))
  return (100*a + 10*b + c)

getBoard1 :: IO Board
getBoard1 = do
  fileContents <- readFile "sudoku.txt"
  let boards = parseBoards $ lines fileContents
  let board1 = boards !! 1
  return board1

main :: IO ()
main = do
  fileContents <- readFile "sudoku.txt"
  let boards = parseBoards $ lines fileContents
  print $ sum (map (fromJust . signature . solve) boards)

