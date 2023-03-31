module Day10 where
import Control.Monad (when)

data Instruction = Noop | Addx Int deriving (Show)

parseInstr :: String -> Instruction
parseInstr "noop" = Noop
parseInstr ('a': 'd': 'd': 'x': ' ': v) = Addx (read v)

instrCycles :: Instruction -> Int
instrCycles Noop = 1
instrCycles (Addx _) = 2

sigCycles :: [Int]
sigCycles = iterate (+40) 20

execute :: (Int, Int) -> Instruction -> (Int, Int)
execute (c, x) instr = (c', x')
    where
        c' = c + instrCycles instr
        x' = case instr of
            Noop -> x
            Addx v -> v + x

xAt :: Int -> [(Int, Int)] -> Int
xAt c = snd . last . takeWhile ((< c) . fst)


solution :: [String] -> Int
solution lines = sum $ map (\c -> c * xAt c execution) cycleSigs
    where
        execution = scanl execute (0, 1) $  map parseInstr lines
        lastCycle = fst (last execution)
        cycleSigs = takeWhile (<= lastCycle) (iterate (+40) 20)