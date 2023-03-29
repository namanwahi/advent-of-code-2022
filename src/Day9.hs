{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Day9 where
import Data.List (nub)
import Debug.Trace (traceShowId)

type Vec = (Int, Int)

dirToVec :: Char -> Vec
dirToVec 'R' = (1, 0)
dirToVec 'L' = (-1, 0)
dirToVec 'U' = (0, 1)
dirToVec 'D' = (0, -1)

add :: Vec -> Vec -> Vec
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sub :: Vec -> Vec -> Vec
sub p1 (x2, y2) = add p1 (-x2, -y2)

areTouching :: Vec -> Vec -> Bool
areTouching p1 p2 = (abs d1 < 2) && (abs d2 < 2)
    where
        (d1, d2) = sub p1 p2

moveKnots :: [Vec] -> Char -> [Vec]
moveKnots (oldHead: rest) c = scanl newTail newHead rest
    where
        newHead = add oldHead (dirToVec c)

newTail :: Vec -> Vec -> Vec
newTail newHead@(hx, hy) tail
    | touching = tail
    | otherwise = (newHx, newHy)
    where
        touching = areTouching newHead tail
        (dx, dy) = sub newHead tail
        newHx = if abs dx >= 2 then hx - (signum dx) else hx
        newHy = if abs dy >= 2 then hy - (signum dy) else hy

solution :: [String] -> Int
solution = length . nub . map last . scanl moveKnots (replicate 10 (0, 0)) . concatMap (\(c : ' ': dig) -> replicate (read dig) c)
