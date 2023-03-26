module Day5 (solution) where
import Data.List.Split ( splitOn )
import Parsing (parseInputLine, integer)
import Data.Either ()
import Data.Maybe ( catMaybes, isJust )
import Text.Parsec (parse, ParseError, many, anyChar)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, anyChar, string)
import Text.Parsec.Combinator (many1, optional, choice)
import Data.List ( transpose )
import GHC.Conc (BlockReason(BlockedOnSTM))
import Data.Sequence (Seq, fromList, index, update)
import Data.Foldable (toList)

type Blocks = Seq [Char]
type Move = (Int, Int, Int)


presentBlock :: Parser (Maybe Char)
presentBlock = do
    char '['
    c <- anyChar
    char ']'
    optional (char ' ')
    return (Just c)

emptyBlock :: Parser (Maybe Char)
emptyBlock = do
    string "   "
    optional (char ' ')
    return Nothing

block :: Parser (Maybe Char)
block = choice [emptyBlock, presentBlock]

blockRow :: Parser [Maybe Char]
blockRow = many1 block

blockStacks :: [[Maybe Char]] -> Blocks
blockStacks  = fromList . map (reverse . catMaybes) . transpose

moveInstruction :: Parser Move
moveInstruction = do
    string "move "
    num <- integer
    string " from "
    start <- integer
    string " to "
    end <- integer
    -- subtract 1 to 0-index the lists
    return (num, start - 1, end - 1)

applyMoves :: Blocks -> [Move] -> Blocks
applyMoves = foldl applyMove
    where
        applyMove :: Blocks -> Move -> Blocks
        applyMove blocks (num, start, end) = update end newEndStack $ update start newStartStack $ blocks
            where
                startStack = index blocks start
                endStack = index blocks end
                (removedBlocks, newStartStack) = splitAt num startStack
                newEndStack = removedBlocks ++ endStack

solution :: [String] -> String
solution lines = map head . toList $ applyMoves blocks moves
    where
        [startingData, moveData] = splitOn [""] lines
        blockData = (tail . reverse) startingData
        moves = map (parseInputLine moveInstruction) moveData
        blocks = blockStacks $ map (parseInputLine blockRow) blockData