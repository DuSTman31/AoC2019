import System.IO
import Data.List.Split
import Data.Maybe
import qualified Data.List as List

-- Generic functions for replacing content of an array at a specific point.
replaceAtInt :: [a] -> Int -> Int -> a -> [a]
replaceAtInt a b c d = if (c == b) then ([d] ++ (tail a)) else ([head a] ++ (replaceAtInt (tail a) b (c + 1) d))

replaceAt :: [a] -> Int -> a -> [a]
replaceAt a b c = replaceAtInt a b 0 c

-- Ensure an array is at least "b" specified length, lengthening it with copies of "c" as necessary.
setMinimumLength_int :: [a] -> Int -> a -> [a]
setMinimumLength_int [] 0 c = []
setMinimumLength_int [] b c = [c] ++ setMinimumLength_int [] (b-1) c
setMinimumLength_int a b c = [head a] ++ setMinimumLength_int (tail a) (b-1) c

setMinimumLength :: [a] -> Int -> a -> [a]
setMinimumLength a b c = if ((length a) >= b) then a else (setMinimumLength_int a b c)

-- Utility functions for handling the memory of the machine
type MemoryElement = Integer
type Memory = [MemoryElement]

-- Utility functions for handling the machine state as a whole
type MachineState = (Memory, Int, [Integer], [Integer], Int)

mMemory :: MachineState -> Memory
mMemory (a, _, _, _, _) = a

mPos :: MachineState -> Int
mPos (_, a, _, _, _) = a

mInput :: MachineState -> [Integer]
mInput (_, _, a, _, _) = a

mOutput :: MachineState -> [Integer]
mOutput (_, _, _, a, _) = a

mRelBase :: MachineState -> Int
mRelBase (_, _, _, _, a) = a

mReplaceMem :: MachineState -> Memory -> MachineState
mReplaceMem (a, b, c, d, e) n = (n, b, c, d, e)

mSetMemory :: Int -> MemoryElement -> MachineState -> MachineState
mSetMemory pos cont a =  mReplaceMem a (replaceAt (mMemory a) pos cont)

mSetIndMemory :: Int -> MemoryElement -> MachineState -> MachineState
mSetIndMemory pos cont a = mSetMemory (fromIntegral (mMemory a !! ((mPos a)+pos))) cont a

mGetIndMemory :: Int ->  MachineState -> MemoryElement
mGetIndMemory pos a = doubleIndirect (mMemory a) ((mPos a) + pos)

mGetRelMemory :: Int -> MachineState -> MemoryElement
mGetRelMemory a b = (mMemory b) !! ((mPos b) + a)

mSetPos :: Int -> MachineState -> MachineState
mSetPos n (a, b, c, d, e) = (a, n, c, d, e)

mAdvPos :: Int -> MachineState -> MachineState
mAdvPos e (a, b, c, d, f) = (a, (b+e), c, d, f)

mReplaceInput :: [Integer] -> MachineState -> MachineState
mReplaceInput a (b, c, d, e, f) = (b, c, a, e, f)

mGetInput :: MachineState -> Integer
mGetInput a = (head (mInput a))

mConsumeInput :: MachineState -> MachineState
mConsumeInput a = mReplaceInput (tail (mInput a)) a

mAddInput :: MachineState -> Integer -> MachineState
mAddInput a i = mReplaceInput ((mInput a) ++ [i]) a

mReplaceOutput :: [Integer] -> MachineState -> MachineState
mReplaceOutput a m = (mMemory m, mPos m, mInput m, a, mRelBase m)

mAddOutput :: Integer -> MachineState -> MachineState
mAddOutput cont a = mReplaceOutput (mOutput a ++ [cont]) a

mGetOutput :: MachineState -> Integer
mGetOutput a = (head (mOutput a))

mConsumeOutput :: MachineState -> MachineState
mConsumeOutput a = mReplaceOutput (tail (mOutput a)) a

mReplaceRelBase :: Int -> MachineState -> MachineState
mReplaceRelBase n (a, b, c, d, e) = (a, b, c, d, n)

mGetOpcode :: MachineState -> Int
mGetOpcode a = fromIntegral (mGetRelMemory 0 a)


-- Functions to implement the memory access modes.
-- memory read functions.
mReadPositionMode :: Int -> MachineState -> MemoryElement
mReadPositionMode pos m = let efPos = (pos + (mPos m))
                              em = setMinimumLength (mMemory m) (efPos+1) 0
                              em2 = setMinimumLength em ((fromIntegral (em !! efPos))+1) 0
                          in em2 !! (fromIntegral (em2 !! efPos))

mReadImmediateMode :: Int -> MachineState -> MemoryElement
mReadImmediateMode pos m = let efPos = (pos + (mPos m))
                           in (mMemory m) !! efPos

mReadRelativeMode :: Int -> MachineState -> MemoryElement
mReadRelativeMode pos m = let efPos = (pos + (mPos m))
                              em = setMinimumLength (mMemory m) (efPos+1) 0
                              em2 = setMinimumLength em (((fromIntegral (em !! efPos)) + (mRelBase m))+1) 0
                          in em2 !! ((fromIntegral (em !! efPos)) + (mRelBase m))

mReadMemModed :: Int -> Int -> MachineState -> MemoryElement
mReadMemModed 0 pos m = mReadPositionMode pos m
mReadMemModed 1 pos m = mReadImmediateMode pos m 
mReadMemModed 2 pos m = mReadRelativeMode pos m

-- memory write functions.

mWritePositionMode :: Int -> MemoryElement -> MachineState -> MachineState
mWritePositionMode pos cont m = let efPos = (pos + (mPos m))
                                    em = setMinimumLength (mMemory m) (efPos+1) 0
                                    em2 = setMinimumLength em ((fromIntegral (em !! efPos)) + 1) 0
                                in mReplaceMem m (replaceAt em2 (fromIntegral (em2 !! efPos)) cont)

mWriteRelativeMode :: Int -> MemoryElement -> MachineState -> MachineState
mWriteRelativeMode pos cont m = let efPos = (pos + (mPos m))
                                    em = setMinimumLength (mMemory m) (efPos+1) 0
                                    em2 = setMinimumLength em ((fromIntegral (em !! efPos) + (mRelBase m))+1) 0
                                in mReplaceMem m (replaceAt em2 (fromIntegral (em2 !! efPos) + (mRelBase m)) cont)

mWriteMemModed :: Int -> Int -> MemoryElement -> MachineState -> MachineState
mWriteMemModed 0 pos cont m = mWritePositionMode pos cont m
mWriteMemModed 2 pos cont m = mWriteRelativeMode pos cont m

mIsTerminated :: MachineState -> Bool
mIsTerminated a = if ((mGetOpcode a) == 99) then True else False

doubleIndirect :: Memory -> Int -> MemoryElement
doubleIndirect a b = a !! (fromIntegral (a !! b))

type Insn = (Int, Int, Int, Int)

addStep :: Insn -> MachineState -> MachineState
addStep (w, x, y, z) a = mAdvPos 4 (mWriteMemModed w 3 ((mReadMemModed y 1 a) + (mReadMemModed x 2 a)) a)

multStep :: Insn -> MachineState -> MachineState
multStep (w, x, y, z) a = mAdvPos 4 (mWriteMemModed w 3 ((mReadMemModed y 1 a) * (mReadMemModed x 2 a)) a)

inputStep :: Insn -> MachineState -> MachineState
inputStep (w, x, y, z) a = mAdvPos 2 (mConsumeInput (mWriteMemModed y 1 (fromIntegral (mGetInput a)) a))

outputStep :: Insn -> MachineState -> MachineState
outputStep (w, x, y, z) a = mAdvPos 2 (mAddOutput (fromIntegral (mReadMemModed y 1 a)) a)

jumpIfTrueStep :: Insn -> MachineState -> MachineState
jumpIfTrueStep (w, x, y, z) a = if ((mReadMemModed y 1 a) /= 0) then (mSetPos (fromIntegral (mReadMemModed x 2 a)) a) else (mAdvPos 3 a)

jumpIfFalseStep :: Insn -> MachineState -> MachineState
jumpIfFalseStep (w, x, y, z) a = if ((mReadMemModed y 1 a) == 0) then (mSetPos (fromIntegral (mReadMemModed x 2 a)) a) else (mAdvPos 3 a)

lessThanStep :: Insn -> MachineState -> MachineState
lessThanStep (w, x, y, z) a = if ((mReadMemModed x 2 a) > (mReadMemModed y 1 a)) then (mAdvPos 4 (mWriteMemModed w 3 1 a)) else (mAdvPos 4 (mWriteMemModed w 3 0 a))

equalsStep :: Insn -> MachineState -> MachineState
equalsStep (w, x, y, z) a = if ((mReadMemModed x 2 a) == (mReadMemModed y 1 a)) then (mAdvPos 4 (mWriteMemModed w 3 1 a)) else (mAdvPos 4 (mWriteMemModed w 3 0 a))

adjustRelativeBaseStep :: Insn -> MachineState -> MachineState
adjustRelativeBaseStep (w, x, y, z) a = mAdvPos 2 (mReplaceRelBase ((mRelBase a) + (fromIntegral (mReadMemModed y 1 a))) a)

executeOpcode :: Insn -> MachineState -> MachineState
executeOpcode (a, b, c, 1) d = addStep (a, b, c, 1) d
executeOpcode (a, b, c, 2) d = multStep (a, b, c, 2) d
executeOpcode (a, b, c, 3) d = inputStep (a, b, c, 3) d
executeOpcode (a, b, c, 4) d = outputStep (a, b, c, 4) d
executeOpcode (a, b, c, 5) d = jumpIfTrueStep (a, b, c, 5) d
executeOpcode (a, b, c, 6) d = jumpIfFalseStep (a, b, c, 6) d
executeOpcode (a, b, c, 7) d = lessThanStep (a, b, c, 7) d
executeOpcode (a, b, c, 8) d = equalsStep (a, b, c, 8) d
executeOpcode (a, b, c, 9) d = adjustRelativeBaseStep (a, b, c, 9) d

padToLength :: Int -> [Int] -> [Int]
padToLength a b = if ((length b) == a) then b else ([0] ++ (padToLength (a-1) b))

intToDigits :: Int -> [Int]
intToDigits a = if (a < 10) then [a] else  (intToDigits (div a 10)) ++ [(mod a 10)]

decodeInstruction :: Int -> (Int, Int, Int, Int)
decodeInstruction a = let d = (padToLength 5 (intToDigits a))
		  in ((d !! 0), (d !! 1), (d !! 2), (((d !! 3) * 10) + (d !! 4)))

step :: MachineState -> MachineState
step a = executeOpcode (decodeInstruction (mGetOpcode a)) a

stepUntilTermination :: MachineState -> MachineState
stepUntilTermination a = if (mGetOpcode a == 99) then a else (stepUntilTermination (step a))

stepUntilOutput :: MachineState -> MachineState
stepUntilOutput a = if ((mGetOpcode a == 99) || ((length (mOutput a)) /= 0) ) then a else (stepUntilOutput (step a)))

type MachineArray = [MachineState]

steps :: Int -> Int -> MachineState -> MachineState
steps a b m = if (a == b) then m else (steps (a+1) b (step m))

main = do
     fHand <- openFile "data/Day9.txt" ReadMode
     contents <- hGetContents fHand
     let input = [(read x :: Integer) | x <- (splitOn ","  contents)]
     print (stepUntilTermination ([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99], 0, [], [], 0))
     print (stepUntilTermination ([1102,34915192,34915192,7,4,7,99,0], 0, [], [], 0))
     print (stepUntilTermination ([104,1125899906842624,99], 0, [], [], 0))
     print (stepUntilTermination (input, 0, [1], [], 0))
     print (stepUntilTermination (input, 0, [2], [], 0))
     hClose fHand
