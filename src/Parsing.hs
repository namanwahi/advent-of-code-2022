module Parsing (parseInputLine, integer) where


import Data.Either

import Text.Parsec (parse, ParseError, many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (sepBy)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


parseInputLine :: Parser a -> String -> a 
parseInputLine parser line = rightOrErr $ regularParse parser line
    where
        rightOrErr :: Either ParseError a -> a 
        rightOrErr (Right a) = a 
        rightOrErr (Left b) = error(show b)


integer :: Parser Int
integer = do { n <- many digit ; return (read n) }
