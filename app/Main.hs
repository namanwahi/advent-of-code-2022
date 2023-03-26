module Main where

import qualified Day1 (solution)

main :: IO ()
main = do
  inputIo <- readFile "app/input.txt"
  let input = lines inputIo
  print (Day1.solution input)
