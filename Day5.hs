import System.IO
import Data.List.Split
import Data.Maybe

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

mGetOpcode :: MachineState -> Int
mGetOpcode a = mGetRelMemory 0 a

mGetInput :: MachineState -> Int
mGetInput a = (head (mInput a))

mConsumeInput :: MachineState -> MachineState
mConsumeInput a = ((mMemory a), (mPos a), (tail (mInput a)), (mOutput a))

mAddOutput :: Int -> MachineState -> MachineState
mAddOutput cont a = ((mMemory a), (mPos a), (mInput a), ((mOutput a) ++ [cont]))

mReadMemModed :: Int -> Int -> MachineState -> Int
mReadMemModed 0 pos m = mGetIndMemory pos m
mReadMemModed 1 pos m = (mMemory m) !! ((mPos m) + pos)

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


doSteps :: MachineState -> MachineState
doSteps a = if (mGetOpcode a == 99) then a else (doSteps (executeOpcode (decodeInstruction (mGetOpcode a)) a))


replaceParams :: MachineState -> Int -> Int -> MachineState
replaceParams a b c = (mSetMemory 1 b (mSetMemory 2 c a))

-- Code for Part 2.

attempt :: MachineState -> Int -> Int -> Bool
attempt a c d = if ((mMemory (doSteps (replaceParams a c d))) !! 0 == 19690720) then True else False

attemptSeq :: MachineState -> Int -> Int -> Maybe (Int, Int)
attemptSeq a c d = if (d == 100) then Nothing else (if (attempt a c d) then Just (c, d) else (attemptSeq a c (d+1)))

attemptSeq2 :: MachineState -> Int -> Maybe (Int, Int)
attemptSeq2 a c = if (c == 100) then Nothing else (if ((isNothing (attemptSeq a c 0) /= True)) then (attemptSeq a c 0) else (attemptSeq2 a (c + 1)))

pout :: Maybe (Int, Int) -> IO ()
pout Nothing = putStrLn "Nothing"
pout (Just a) = do
     putStr "Just "
     putStrLn (show a)

main = do
     fHand <- openFile "data/Day5.txt" ReadMode
     contents <- hGetContents fHand
     let input = [(read x :: Int) | x <- (splitOn ","  contents)]
     print (doSteps (input, 0, [1], []))
     print (doSteps ([3,9,8,9,10,9,4,9,99,-1,8], 0, [1], []))
     print (doSteps ([3,9,8,9,10,9,4,9,99,-1,8], 0, [8], []))
     print (doSteps ([3,9,7,9,10,9,4,9,99,-1,8], 0, [1], []))
     print (doSteps ([3,9,7,9,10,9,4,9,99,-1,8], 0, [8], []))
     print (doSteps ([3,3,1108,-1,8,3,4,3,99], 0, [1], []))
     print (doSteps ([3,3,1108,-1,8,3,4,3,99], 0, [8], []))
     print (doSteps ([3,3,1107,-1,8,3,4,3,99], 0, [1], []))
     print (doSteps ([3,3,1107,-1,8,3,4,3,99], 0, [8], []))		
     print (doSteps (input, 0, [5], []))     
--     print (mGetIndMemory 1 ([2, 0, 0, 0, 1, 0, 0, 0, 99], 4, [], []))
--     print (doSteps ([1,0,0,0,1,0,0,0,99], 0, [], []))
--     print (doSteps (replaceParams (input, 0, [], []) 12 2))
--     pout (attemptSeq2 (input, 0, [], []) 0)
     hClose fHand
