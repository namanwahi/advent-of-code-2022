module Day6 (solution) where
import Data.List (nub)

processSignal :: Int -> String -> Int
processSignal stride = processSignal' 0
    where 
        processSignal' ind signal
            | unique = ind + stride 
            | otherwise = processSignal' (ind + 1) (tail signal)
            where
                window = take stride signal
                unique = length (nub window) == length window

solution :: [String] -> [Int]
solution = map (processSignal 14)