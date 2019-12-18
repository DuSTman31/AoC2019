import System.IO
import Data.List.Split
import Data.Maybe
import qualified Data.List as List

-- Generic functions for replacing content of an array at a specific point.
replaceAtInt :: [a] -> Int -> Int -> a -> [a]
replaceAtInt a b c d = if (c == b) then ([d] ++ (tail a)) else ([head a] ++ (replaceAtInt (tail a) b (c + 1) d))

replaceAt :: [a] -> Int -> a -> [a]
replaceAt a b c = replaceAtInt a b 0 c

-- Utility functions for handling the memory of the machine
type Memory = [Int]

-- Utility functions for handling the machine state as a whole
type MachineState = (Memory, Int, [Int], [Int])

mMemory :: MachineState -> [Int]
mMemory (a, _, _, _) = a

mPos :: MachineState -> Int
mPos (_, a, _, _) = a

mInput :: MachineState -> [Int]
mInput (_, _, a, _) = a

mOutput :: MachineState -> [Int]
mOutput (_, _, _, a) = a

mReplaceMem :: MachineState -> Memory -> MachineState
mReplaceMem (a, b, c, d) e = (e, b, c, d)

mSetMemory :: Int -> Int -> MachineState -> MachineState
mSetMemory pos cont a =  mReplaceMem a (replaceAt (mMemory a) pos cont)

mSetIndMemory :: Int -> Int -> MachineState -> MachineState
mSetIndMemory pos cont a = mSetMemory (mMemory a !! ((mPos a)+pos)) cont a

mGetIndMemory :: Int ->  MachineState -> Int
mGetIndMemory pos a = doubleIndirect (mMemory a) ((mPos a) + pos)

mGetRelMemory :: Int -> MachineState -> Int
mGetRelMemory a b = (mMemory b) !! ((mPos b) + a)

mSetPos :: Int -> MachineState -> MachineState
mSetPos e (a, b, c, d) = (a, e, c, d)

mAdvPos :: Int -> MachineState -> MachineState
mAdvPos e (a, b, c, d) = (a, (b+e), c, d)

mSetInput :: [Int] -> MachineState -> MachineState
mSetInput a (b, c, d, e) = (b, c, a, e)

mGetOpcode :: MachineState -> Int
mGetOpcode a = mGetRelMemory 0 a

mGetInput :: MachineState -> Int
mGetInput a = (head (mInput a))

mConsumeInput :: MachineState -> MachineState
mConsumeInput a = ((mMemory a), (mPos a), (tail (mInput a)), (mOutput a))

mAddInput :: MachineState -> Int -> MachineState
mAddInput a i = ((mMemory a), (mPos a), ((mInput a) ++ [i]), (mOutput a))

mAddOutput :: Int -> MachineState -> MachineState
mAddOutput cont a = ((mMemory a), (mPos a), (mInput a), ((mOutput a) ++ [cont]))

mGetOutput :: MachineState -> Int
mGetOutput a = (head (mOutput a))

mConsumeOutput :: MachineState -> MachineState
mConsumeOutput a = ((mMemory a), (mPos a), (mInput a), (tail (mOutput a)))

mReadMemModed :: Int -> Int -> MachineState -> Int
mReadMemModed 0 pos m = mGetIndMemory pos m
mReadMemModed 1 pos m = (mMemory m) !! ((mPos m) + pos)

mIsTerminated :: MachineState -> Bool
mIsTerminated a = if ((mGetOpcode a) == 99) then True else False

doubleIndirect :: [Int] -> Int -> Int
doubleIndirect a b = a !! (a !! b)

type Insn = (Int, Int, Int, Int)

addStep :: Insn -> MachineState -> MachineState
addStep (w, x, y, z) a = mAdvPos 4 (mSetIndMemory 3 ((mReadMemModed y 1 a) + (mReadMemModed x 2 a)) a)

multStep :: Insn -> MachineState -> MachineState
multStep (w, x, y, z) a = mAdvPos 4 (mSetIndMemory 3 ((mReadMemModed y 1 a) * (mReadMemModed x 2 a)) a)

inputStep :: Insn -> MachineState -> MachineState
inputStep (w, x, y, z) a = mAdvPos 2 (mConsumeInput (mSetIndMemory 1 (mGetInput a) a))

outputStep :: Insn -> MachineState -> MachineState
outputStep (w, x, y, z) a = mAdvPos 2 (mAddOutput (mReadMemModed y 1 a) a)

jumpIfTrueStep :: Insn -> MachineState -> MachineState
jumpIfTrueStep (w, x, y, z) a = if ((mReadMemModed y 1 a) /= 0) then (mSetPos (mReadMemModed x 2 a) a) else (mAdvPos 3 a)

jumpIfFalseStep :: Insn -> MachineState -> MachineState
jumpIfFalseStep (w, x, y, z) a = if ((mReadMemModed y 1 a) == 0) then (mSetPos (mReadMemModed x 2 a) a) else (mAdvPos 3 a)

lessThanStep :: Insn -> MachineState -> MachineState
lessThanStep (w, x, y, z) a = if ((mReadMemModed x 2 a) > (mReadMemModed y 1 a)) then (mAdvPos 4 (mSetIndMemory 3 1 a)) else (mAdvPos 4 (mSetIndMemory 3 0 a))

equalsStep :: Insn -> MachineState -> MachineState
equalsStep (w, x, y, z) a = if ((mReadMemModed x 2 a) == (mReadMemModed y 1 a)) then (mAdvPos 4 (mSetIndMemory 3 1 a)) else (mAdvPos 4 (mSetIndMemory 3 0 a))

executeOpcode :: Insn -> MachineState -> MachineState
executeOpcode (a, b, c, 1) d = addStep (a, b, c, 1) d
executeOpcode (a, b, c, 2) d = multStep (a, b, c, 2) d
executeOpcode (a, b, c, 3) d = inputStep (a, b, c, 3) d
executeOpcode (a, b, c, 4) d = outputStep (a, b, c, 4) d
executeOpcode (a, b, c, 5) d = jumpIfTrueStep (a, b, c, 5) d
executeOpcode (a, b, c, 6) d = jumpIfFalseStep (a, b, c, 6) d
executeOpcode (a, b, c, 7) d = lessThanStep (a, b, c, 7) d
executeOpcode (a, b, c, 8) d = equalsStep (a, b, c, 8) d

padToLength :: Int -> [Int] -> [Int]
padToLength a b = if ((length b) == a) then b else ([0] ++ (padToLength (a-1) b))

intToDigits :: Int -> [Int]
intToDigits a = if (a < 10) then [a] else  (intToDigits (div a 10)) ++ [(mod a 10)]

decodeInstruction :: Int -> (Int, Int, Int, Int)
decodeInstruction a = let d = (padToLength 5 (intToDigits a))
		  in ((d !! 0), (d !! 1), (d !! 2), (((d !! 3) * 10) + (d !! 4)))


stepUntilTermination :: MachineState -> MachineState
stepUntilTermination a = if (mGetOpcode a == 99) then a else (stepUntilTermination (executeOpcode (decodeInstruction (mGetOpcode a)) a))

stepUntilOutput :: MachineState -> MachineState
stepUntilOutput a = if ((mGetOpcode a == 99) || ((length (mOutput a)) /= 0) ) then a else (stepUntilOutput (executeOpcode (decodeInstruction (mGetOpcode a)) a))

type MachineArray = [MachineState]

-- doSequence - Part 1 solution - chain output from five programs.
doSequence :: [Int] -> MachineState -> MachineState
doSequence a b = (stepUntilTermination (mSetInput [a !! 0, (mOutput (stepUntilTermination (mSetInput [a !! 1, (mOutput (stepUntilTermination (mSetInput [a !! 2, (mOutput (stepUntilTermination (mSetInput [a !! 3, (mOutput (stepUntilTermination (mSetInput [a !! 4, 0] b))) !! 0] b))) !! 0] b))) !! 0] b))) !! 0] b))

doubles :: [Int] -> Int -> Int
doubles a 2 = if (a !! 0 == a !! 1) then 1 else 0
doubles a d = if (a !! 0 == a !! 1) then ((doubles (tail a) (d-1)) + 1) else (doubles (tail a) (d -1))

areDigitsUnique :: [Int] -> Bool
areDigitsUnique [] = True
areDigitsUnique a = if ((doubles (List.sort a) (length a)) > 0) then False else True

generateRange :: Int -> Int -> Int -> [Int] -> [[Int]]
generateRange a r1 r2 b = if (a == 1) then ([(b ++ [x])| x <- [r1..r2]]) else (foldl1 (\x y -> x ++ y) (map (\x -> (generateRange (a-1) r1 r2 (b ++ [x]))) [r1..r2] ))

generateUniqueRange :: Int -> Int -> Int -> [[Int]]
generateUniqueRange a r1 r2 = filter areDigitsUnique (generateRange a r1 r2 [])

-- part 2


-- function to take output from one machine and insert it to the input queue of next machine.
outputToInput :: MachineArray -> Int -> Int -> MachineArray
outputToInput a i j = replaceAt (replaceAt a j (mAddInput (a !! j) (mGetOutput (a !! i)))) i (mConsumeOutput (a !! i))

doMultiMachineCycle :: MachineArray -> MachineArray
doMultiMachineCycle [] = []
doMultiMachineCycle a = let step = (\b i -> (outputToInput (replaceAt b i (stepUntilOutput (b!!i)))) i (i+1))
                            fourSteps = (step (step (step (step a 0) 1) 2) 3)
                        in (replaceAt fourSteps 4 (stepUntilOutput (fourSteps !! 4)))



doSeqUntilCompletion :: MachineArray -> MachineArray
doSeqUntilCompletion a = if (mIsTerminated (a !! 4)) then a else (doSeqUntilCompletion (doMultiMachineCycle (outputToInput a 4 0)))

p2 :: [Int] -> MachineArray -> MachineArray
p2 a b = (doSeqUntilCompletion [mAddInput (b!!0) (a!!0), mAddInput (b!!1) (a!!1), mAddInput (b!!2) (a!!2), mAddInput (b!!3) (a!!3), mAddOutput 0 (mAddInput (b!!4) (a!!4))])

main = do
     fHand <- openFile "data/Day7.txt" ReadMode
     contents <- hGetContents fHand
     let input = [(read x :: Int) | x <- (splitOn ","  contents)]
--     print (doSequence [0, 1, 2, 3, 4] ([3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] , 0, [], []))
--     print (map (\x -> (doSequence x ([3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0], 0, [], []))) (generateUniqueRange 5))
     print (foldl1 (\x y -> (if (((mOutput x) !! 0) > ((mOutput y) !! 0)) then x else y)) (map (\x -> doSequence x (input, 0, [], [])) (generateUniqueRange 5 0 4)))
     print (foldl1 (\x y -> (if (((mOutput (x!!4)) !! 0) > ((mOutput (y!!4)) !! 0)) then x else y)) (map (\x -> p2 x (replicate 5 (input, 0, [], []))) (generateUniqueRange 5 5 9)))
     hClose fHand
