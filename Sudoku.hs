
import Array ( Array
             , array
             , elems
             , (!))
import Data.List (nub)
import Data.Maybe (isJust, fromJust)
import Debug.Trace (traceShow)

-- set up the data structures

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

-- deconstruct a board into rows/columns/squares

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

-- solving
    
-- | takes a row/column/square and returns a list of all the numbers that are already taken
taken :: [Cell] -> [Int]
taken row = do
  cell <- row
  n <- value cell
  return n

solveRow :: [Cell] -> [Cell]
solveRow row = map removeTaken row
  where
    removeTaken c@(Cell [_]) = c
    removeTaken cell = Cell $ filter (`notElem` takens) (values cell)
    takens = taken row

pass :: ([Cell] -> [Cell]) -> Board -> Board
pass f = (unrows . f' . rows) . (uncolumns . f' . columns) . (unsquares . f' . squares)
  where f' = map f

solveWith :: ([Cell] -> [Cell]) -> Board -> Board
solveWith f board = let board' = pass f board in
  if board == board' then board' else solveWith f board'

naiveSolve :: Board -> Board
naiveSolve = solveWith solveRow

solveRow' :: [Cell] -> [Cell]
solveRow' row = (traceShow unusedNums) row where
  unfilled = filter (not . filled) row
  unusedNums = nub $ concatMap values unfilled

solve :: Board -> Board
solve = solveWith solveRow'

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
  boards <- getBoards
  return (boards !! 1)

getBoards :: IO [Board]
getBoards = do
  fileContents <- readFile "sudoku.txt"
  return $ parseBoards (lines fileContents)

naiveMain :: IO ()
naiveMain = do
  boards <- getBoards
  print $ map naiveSolve boards

main :: IO ()
main = do
  boards <- getBoards
  print $ sum (map (fromJust . signature . solve) boards)
