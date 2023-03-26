module Day1 (solution) where

solution :: [String] -> Int
solution = maximum . foldl addCalories [0]
    where
        addCalories:: [Int] -> String -> [Int]
        addCalories calorieSums "" = 0 : calorieSums
        addCalories (currenSum: calorieSums) calStr = (currenSum + read calStr) : calorieSums 
