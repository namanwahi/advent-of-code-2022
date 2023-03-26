module Day1 (solution) where
import Data.List

solution :: [String] -> Int
solution = sum . take 3. reverse . sort . foldl addCalories [0]
    where
        addCalories:: [Int] -> String -> [Int]
        addCalories calorieSums "" = 0 : calorieSums
        addCalories (currenSum: calorieSums) calStr = (currenSum + read calStr) : calorieSums
