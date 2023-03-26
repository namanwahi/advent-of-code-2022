module Day3 (solution) where
import Data.List
import Data.List.Split (chunksOf)
import Data.Char

priority :: Char -> Int
priority item
    | isUpper item = ord item - ord 'A' + 27
    | otherwise = ord item - ord 'a' + 1

solution :: [String] -> Int
solution = sum . map (priority . head . foldl1 intersect) . chunksOf 3