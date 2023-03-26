module Day2 (solution) where
import Data.List

data Shape = Rock | Paper | Scissors deriving Eq

-- score of the shape per round
shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

-- get the shape that a shape beats
beats :: Shape -> Shape
beats Rock = Scissors
beats Scissors = Paper
beats Paper = Rock

-- score for an outcome of a round of RPS
outcomeScore :: Shape -> Shape -> Int
outcomeScore oppShape myShape
    | oppShape == myShape = 3
    | beats myShape == oppShape = 6
    | otherwise = 0

-- oponent shape code to shape
opponentShape :: Char -> Shape
opponentShape 'A' = Rock
opponentShape 'B' = Paper
opponentShape 'C' = Scissors

-- my shape code to shape
myShape :: Char -> Shape
myShape 'X' = Rock
myShape 'Y' = Paper
myShape 'Z' = Scissors

solution :: [String] -> Int
solution = sum . map roundScore
    where
        roundScore:: String -> Int
        roundScore [oppCode, ' ', myCode] = outcomeScore opp me + shapeScore me
            where
                opp = opponentShape oppCode
                me = myShape myCode