module Day7 where
import Parsing (integer, parseInputLine)
import Text.Parsec (parse, ParseError, many, anyChar)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string)
import Text.Parsec.Combinator ( anyToken , many1, choice)
import Data.List.Split (splitOn)
import Data.Bifoldable (biList)
import qualified Data.Bifunctor
import Data.List (nub)
import Data.Maybe (fromJust)

data Line = Cd String | Ls | Dir String | File String Int deriving (Show, Eq)
type Path = [String]
type PathToSize = [(Path, Int)]

buildFiles :: [Line] -> PathToSize
buildFiles =  map (Data.Bifunctor.first reverse) . buildFiles' [] []
    where
        buildFiles' :: PathToSize -> Path -> [Line] -> PathToSize
        -- base case - return acc
        buildFiles' acc _ [] = acc
        -- root -  reset path 
        buildFiles' acc path (Cd "/": lines) = buildFiles' acc ["/"] lines
        -- up dir - pop path
        buildFiles' acc path (Cd "..": lines) = buildFiles' acc (tail path) lines
        -- down dir - push t0 path
        buildFiles' acc path (Cd dir: lines) = buildFiles' acc (dir: path) lines
        -- file - add full path to acc
        buildFiles' acc path (File name size: lines) = buildFiles' ((name : path, size): acc) path lines
        -- else
        buildFiles' acc path (_: lines) = buildFiles' acc path lines

allDirs :: PathToSize -> [Path]
allDirs = nub . filter (not . null) . concatMap (tail . takeWhile (not . null) . iterate init . fst)

parseLine :: String -> Line
parseLine ('$': ' ': 'c': 'd': ' ': dir) = Cd dir
parseLine "$ ls" = Ls
parseLine ('d': 'i': 'r': ' ': dir) = Dir dir
parseLine file = File name (read sizeStr)
    where
        [sizeStr, name] = splitOn " " file

dirIsParent :: Path -> Path -> Bool
dirIsParent dirPath filePath
    | null match = False
    | otherwise = and match
    where
        match = zipWith (==) dirPath filePath

dirToSize :: PathToSize -> PathToSize
dirToSize fileToSize = map (\d -> (d, getSize d)) dirs
    where
        dirs = allDirs fileToSize
        getSize dir = sum $ map snd $ filter (dirIsParent dir . fst) fileToSize


solution :: [String] -> Int
-- part 1 
-- solution = sum . map snd . filter ((<= 100000) .  snd) . dirToSize . buildFiles . map parseLine
-- part 2
solution lines = minimum $ filter (>= toDelete) $ map snd dirSizes
    where 
        dirSizes = (dirToSize . buildFiles . map parseLine) lines
        totalSize = fromJust $ lookup ["/"] dirSizes
        unusedSpace = 70000000 - totalSize
        toDelete = 30000000 - unusedSpace