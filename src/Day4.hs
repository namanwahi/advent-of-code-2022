module Day4 (solution) where

import Parsing
import Data.Either

import Text.Parsec (parse, ParseError, many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char)
import Data.List

type Section = (Int, Int)

overlap :: Section -> Section -> Bool
overlap (x1, x2) (y1, y2) = (not . null) $ intersect [x1..x2] [y1..y2]

sectionPair :: Parser (Section, Section)
sectionPair = do
    x1 <- integer
    char '-'
    y1 <- integer
    char ','
    x2 <- integer
    char '-'
    y2 <- integer
    return ((x1, y1), (x2, y2))


solution :: [String] -> Int
solution = length . filter (uncurry overlap) . map (parseInputLine sectionPair)