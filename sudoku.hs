import Data.List

-- represent a sudoku grid as a list of rows
-- each row is a list of Chars i.e. a string
type Row = [Char]
type Grid = [Row]

-- This is the initial grid
-- We have to replace all the zeros to get a solution
-- startingGrid :: Grid
-- startingGrid = ["003020600","900305001","001806400","008102900","700000008","006708200","002609500","800203009","005010300"]

-- Test for a complete solution
-- We need every row, column, and 3x3 cell to contain '1'..'9' exactly once
completeGrid :: Grid -> Bool
completeGrid grid = completeRows grid && completeCols grid && completeCells grid

-- test whether every row is complete
completeRows :: Grid -> Bool
completeRows grid = and $ map complete grid

-- test whether every column is complete
completeCols :: Grid -> Bool
completeCols grid = and $ map complete $ transpose grid

-- test whether every 3x3 cell is complete
completeCells :: Grid -> Bool
completeCells grid = and $ map complete $ getCells grid

-- get a list of 3x3 cells from a grid
getCells :: Grid -> Grid
getCells grid =
  (concatMap (take 3) g1):
  (concatMap ((take 3).(drop 3)) g1):
  (concatMap (drop 6) g1):
  (concatMap (take 3) g2):
  (concatMap ((take 3).(drop 3)) g2):
  (concatMap (drop 6) g2):
  (concatMap (take 3) g3):
  (concatMap ((take 3).(drop 3)) g3):
  [(concatMap (drop 6) g3)]
    where g1 = take 3 grid
          g2 = take 3 $ drop 3 grid
          g3 = drop 6 grid

-- check a row/column/cell cor completeness
complete :: [Char] -> Bool
complete xs = sort xs == "123456789"

-- Test for a partial solution
-- We need every row, column, and 3x3 cell to contain no repeated chars in the range '1'..'9'
validGrid :: Grid -> Bool
validGrid grid = validRows grid && validCols grid && validCells grid

validRows :: Grid -> Bool
validRows grid = and $ map valid grid

validCols :: Grid -> Bool
validCols grid = and $ map valid $ transpose grid

validCells :: Grid -> Bool
validCells grid = and $ map valid $ getCells grid

valid :: [Char] -> Bool
valid row = (length r) == (length $ nub r)
  where r = filter (/='0') row

-- flatten a grid into a single string
flatten :: Grid -> [Char]
flatten = concat

-- unflatten a string of 81 chars back into a grid
unflatten :: [Char] -> Grid
unflatten [] = []
unflatten xs = (take 9 xs):(unflatten $ drop 9 xs)

-- return a list of grids with the first '0' replaced by each possible replacement char
validPartialSolutions :: Grid -> [Grid]
validPartialSolutions grid = filter validGrid grids
  where g = flatten grid
        gs = map (replaceFirstZero g) ['1'..'9']
        grids = map unflatten gs

-- find first '0' in a string and replace it with the given parameter
replaceFirstZero :: [Char] -> Char -> [Char]
replaceFirstZero [] _ = []
replaceFirstZero (x:xs) c = if x == '0' then c:xs else x:(replaceFirstZero xs c)

solve :: Grid -> [Grid]
solve g
    | completeGrid g = [g]
    | otherwise      = concatMap solve psols
  where
    psols = validPartialSolutions g

makeGrids :: [[Char]] -> [Grid]
makeGrids [] = []
makeGrids xs = (take 9 xs):(makeGrids $ drop 9 xs)

main = do
  contents <- readFile "sudoku.txt"
  mapM_ putStrLn $ map (unlines . head . solve) $ makeGrids $ lines contents


