module Day8 where
import Data.Char (digitToInt)
import Data.Bits (Bits(xor))
import Debug.Trace (trace, traceShowId)

-- grid, width, height
type Grid = [[Int]]

dims :: Grid -> (Int, Int)
dims grid = (w, h)
    where
        w = length (head grid)
        h = length grid


allCoords :: Grid -> [(Int, Int)]
allCoords grid = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
    where
        (w, h) = dims grid

getPoint :: Grid -> (Int, Int) -> Int
getPoint grid (x, y) = (grid !! y) !! x

getRow :: Grid -> Int -> [Int]
getRow grid y = grid !! y

getColumn :: Grid -> Int -> [Int]
getColumn grid x = map (!! x) grid

viewingDistance :: Int -> [Int] -> Int
viewingDistance _ [] = 0
viewingDistance h view
    | null search = length view
    | otherwise = fst $ head search
    where
        search = filter (\(i, h') -> h <= h') $ zip [1..] view

score :: Grid -> (Int, Int) -> Int
score grid p@(x, y) = product $ map (traceShowId . viewingDistance point) [beforeCol, beforeRow, afterCol, afterRow]
    where
        -- height at point 
        point = getPoint grid p
        -- column at point 
        col = getColumn grid x
        -- row at point 
        row = getRow grid y
        -- elements before point in col
        beforeCol = reverse $ take y col
        -- elements before point in row
        beforeRow = reverse $ take x row
        -- elements after point in col
        afterCol = drop (y + 1) col
        -- elements before point in row 
        afterRow = drop (x + 1) row


solution :: [String] -> Int
solution lines = maximum $ map (score grid) $ allCoords grid
    where
        grid = map (map digitToInt) lines
