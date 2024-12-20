import Data.List.Extra ( splitOn, intercalate )
import Data.Tuple.Extra ( fst3, snd3, thd3 )
import Data.Char ( isDigit )
import Data.IntMap.Strict ( IntMap )
import Data.IntMap.Strict qualified as M
import Data.Maybe ( fromJust, isNothing, isJust )
import Data.Bits ( xor )

type Registers = (Integer, Integer, Integer)
type Instructions = IntMap (Int, Int)
data ProgramState = ProgramState {
    insPointer :: Int,
    registers :: Registers,
    output :: [String],
    program :: Instructions
} deriving (Show)

parseInput :: String -> String -> ProgramState
parseInput rs is = ProgramState 0 (registerA, 0, 0) [] instructions
    where registerA = read . takeWhile (/='\n') . dropWhile (not.isDigit) $ rs
          instructions = M.fromAscList $ zip [0..] (getPairs.splitOn ",".drop (length "Program: ") $ is)
          getPairs :: [String] -> [(Int, Int)]
          getPairs [] = []
          getPairs [x] = error "Instruction count is uneven"
          getPairs (x:y:ys) = (read x, read y) : getPairs ys

evalComboOperand :: Int -> ProgramState -> Integer
evalComboOperand op s
    | op < 4 = fromIntegral op
    | op == 4 = fst3.registers $ s
    | op == 5 = snd3.registers $ s
    | op == 6 = thd3.registers $ s
    | op == 7 = error "Reserved combo operand 7 used in program."

step :: ProgramState -> Maybe ProgramState
step s
    | isNothing instructionM = Nothing
    | ins == 3 && regA == 0 = Just (ProgramState (insPointer s + 1) (registers s) (output s) (program s))
    | ins == 0 = intoRegisterA (regA `quot` (2 ^ comboOp))
    | ins == 1 = intoRegisterB (regB `xor` literalOp)
    | ins == 2 = intoRegisterB (comboOp `mod` 8)
    | ins == 3 = Just (ProgramState operand (registers s) (output s) (program s))
    | ins == 4 = intoRegisterB (regB `xor` regC)
    | ins == 5 = Just (ProgramState (insPointer s + 1) (registers s) (show (comboOp `mod` 8) : output s) (program s))
    | ins == 6 = intoRegisterB (regA `quot` (2 ^ comboOp))
    | ins == 7 = intoRegisterC (regA `quot` (2 ^ comboOp))
    where instructionM = M.lookup (insPointer s) $ program s
          (ins, operand) = fromJust instructionM
          literalOp = fromIntegral operand
          comboOp = evalComboOperand operand s
          regA = fst3.registers $ s
          regB = snd3.registers $ s
          regC = thd3.registers $ s
          intoRegisterA x = Just (ProgramState (insPointer s + 1) (x, regB, regC) (output s) (program s))
          intoRegisterB x = Just (ProgramState (insPointer s + 1) (regA, x, regC) (output s) (program s))
          intoRegisterC x = Just (ProgramState (insPointer s + 1) (regA, regB, x) (output s) (program s))

execute :: ProgramState -> ProgramState
execute s = fromJust . last . takeWhile isJust . iterate (>>= step) $ Just s

main = do
    [registers, program] <- splitOn "\n\n" <$> readFile "inputs/day17-test.txt"
    --traverse print $ takeWhile isJust . iterate (>>=step) . Just $ parseInput registers program
    putStrLn $ intercalate "," . reverse.output.execute $ parseInput registers program
