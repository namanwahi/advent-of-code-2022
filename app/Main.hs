module Main where

import qualified Day1 (solution)
import qualified Day2 (solution)
import qualified Day3 (solution)
import qualified Day4 (solution)

main :: IO ()
main = do
  inputIo <- readFile "app/input.txt"
  let input = lines inputIo
  print (Day4.solution input)
