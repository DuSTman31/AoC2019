import System.IO
import Data.List.Split
import Data.Maybe
import qualified Data.List as List
import Control.Exception
import Control.DeepSeq
import qualified Data.Map as Map

-- Generic functions for replacing content of an array at a specific point.
replaceAt_int :: [a] -> Int -> a -> [a]
replaceAt_int a 0 d = [d] ++ (tail a)
replaceAt_int a b d = [head a] ++ (replaceAt_int (tail a) (b-1) d)

replaceAt :: [a] -> Int -> a -> [a]
replaceAt a b c = let i = splitAt b a 
                      x = (\(y, z) -> y ++ [c] ++ tail z)
                  in x i

-- Ensure an array is at least "b" specified length, lengthening it with copies of "c" as necessary.
setMinimumLength_int :: [a] -> Int -> a -> [a]
setMinimumLength_int [] 0 c = []
setMinimumLength_int [] b c = [c] ++ setMinimumLength_int [] (b-1) c
setMinimumLength_int a b c = [head a] ++ setMinimumLength_int (tail a) (b-1) c

setMinimumLength :: [a] -> Int -> a -> [a]
setMinimumLength a b c = if ((length a) >= b) then a else (setMinimumLength_int a b c)

-- Utility functions for handling the memory of the machine
type MemoryElement = Integer
type Memory = (Map.Map Integer Integer, Integer)

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


mMemoryReserve_int :: Memory -> Integer -> Memory
mMemoryReserve_int m i = if (snd m < i) then mMemoryReserve_int ((Map.insert (snd m) 0 (fst m)), ((snd m) + 1)) i else m

mMemoryReserve :: MachineState -> Integer -> MachineState
mMemoryReserve m i = let mem = mMemory m
                     in if (snd mem < i) then mReplaceMem  m (mMemoryReserve_int mem i) else m


mMemPrimWrite :: MemoryElement -> Integer -> Memory -> Memory
mMemPrimWrite cont i m = ((Map.insert i cont (fst m)), snd m)

mMemPrimRead :: Integer -> Memory -> MemoryElement
mMemPrimRead i m = let c = Map.lookup i (fst m)
                   in if (isJust c) then fromJust c else 0

mGetRelMemory :: Int -> MachineState -> MemoryElement
mGetRelMemory a b = let c = Map.lookup (fromIntegral ((mPos b) + a)) (fst (mMemory b))
                    in if (isJust c) then fromJust c else 0

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
mReadPositionMode pos m = let efPos = fromIntegral (pos + (mPos m))
                              em = mMemoryReserve m (efPos+1)
                              em2 = mMemoryReserve em ((mMemPrimRead efPos (mMemory em))+1)
                          in mMemPrimRead (mMemPrimRead efPos (mMemory em2)) (mMemory em2)

mReadImmediateMode :: Int -> MachineState -> MemoryElement
mReadImmediateMode pos m = let efPos = fromIntegral (pos + (mPos m))
                           in mMemPrimRead efPos (mMemory m)

mReadRelativeMode :: Int -> MachineState -> MemoryElement
mReadRelativeMode pos m = let efPos = fromIntegral (pos + (mPos m))
                              em = mMemoryReserve m (efPos+1) 
                              em2 = mMemoryReserve em (( (mMemPrimRead efPos (mMemory em)) + (fromIntegral (mRelBase m)))+1)
                          in mMemPrimRead (( (mMemPrimRead efPos (mMemory em2))) + (fromIntegral (mRelBase m))) (mMemory em2) 

mReadMemModed :: Int -> Int -> MachineState -> MemoryElement
mReadMemModed 0 pos m = mReadPositionMode pos m
mReadMemModed 1 pos m = mReadImmediateMode pos m 
mReadMemModed 2 pos m = mReadRelativeMode pos m

-- memory write functions.

mWritePositionMode :: Int -> MemoryElement -> MachineState -> MachineState
mWritePositionMode pos cont m = let efPos = fromIntegral (pos + (mPos m))
                                    em = mMemoryReserve m (efPos+1)
                                    em2 = mMemoryReserve em ((fromIntegral (mMemPrimRead efPos (mMemory em))) + 1)
                                    pointer = mMemPrimRead efPos (mMemory em2)
                                in mReplaceMem m (mMemPrimWrite cont pointer (mMemory em2))

mWriteRelativeMode :: Int -> MemoryElement -> MachineState -> MachineState
mWriteRelativeMode pos cont m = let efPos = fromIntegral (pos + (mPos m))
                                    memcont = (\x -> fst (mMemory x))
                                    em = mMemoryReserve m (efPos+1)
                                    em2 = mMemoryReserve em ((mMemPrimRead efPos (mMemory em) + (fromIntegral (mRelBase m)))+1)
                                in mReplaceMem m ((Map.insert (mMemPrimRead efPos (mMemory em2) + (fromIntegral (mRelBase m))) cont (memcont em2)), snd (mMemory em2))

mWriteMemModed :: Int -> Int -> MemoryElement -> MachineState -> MachineState
mWriteMemModed 0 pos cont m = mWritePositionMode pos cont m
mWriteMemModed 2 pos cont m = mWriteRelativeMode pos cont m

mIsTerminated :: MachineState -> Bool
mIsTerminated a = if ((mGetOpcode a) == 99) then True else False

mListToMemory :: [MemoryElement] -> Memory
mListToMemory me = (Map.fromList (zip (map fromIntegral [0..((length me)-1)]) me), fromIntegral (length me))


type Insn = (Int, Int, Int, Int)

addStep :: Insn -> MachineState -> MachineState
addStep (w, x, y, z) a = let ans = (mWriteMemModed w 3 ((mReadMemModed y 1 a) + (mReadMemModed x 2 a)) a)
                         in ans `seq` mAdvPos 4 ans

multStep :: Insn -> MachineState -> MachineState
multStep (w, x, y, z) a = let ans = (mWriteMemModed w 3 ((mReadMemModed y 1 a) * (mReadMemModed x 2 a)) a)
                          in ans `seq` mAdvPos 4 ans

inputStep :: Insn -> MachineState -> MachineState
inputStep (w, x, y, z) a = mAdvPos 2 (mConsumeInput (mWriteMemModed y 1 (fromIntegral (mGetInput a)) a))

outputStep :: Insn -> MachineState -> MachineState
outputStep (w, x, y, z) a = mAdvPos 2 (mAddOutput (fromIntegral (mReadMemModed y 1 a)) a)

jumpIfTrueStep :: Insn -> MachineState -> MachineState
jumpIfTrueStep (w, x, y, z) a = if ((mReadMemModed y 1 a) /= 0) then (mSetPos (fromIntegral (mReadMemModed x 2 a)) a) else (mAdvPos 3 a)

jumpIfFalseStep :: Insn -> MachineState -> MachineState
jumpIfFalseStep (w, x, y, z) a = if ((mReadMemModed y 1 a) == 0) then (mSetPos (fromIntegral (mReadMemModed x 2 a)) a) else (mAdvPos 3 a)

lessThanStep :: Insn -> MachineState -> MachineState
lessThanStep (w, x, y, z) a = let op = (\o -> (mAdvPos 4 (mWriteMemModed w 3 o a)))
                                  op1 = op 1
                                  op2 = op 0
                              in if ((mReadMemModed x 2 a) > (mReadMemModed y 1 a)) then op1 `seq` op1 else op2 `seq` op2

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
stepUntilTermination a = let sa = step a
                         in if (mGetOpcode sa == 99) then sa else sa `seq` stepUntilTermination sa

stepUntilOutput :: MachineState -> MachineState
stepUntilOutput a = if ((mGetOpcode a == 99) || ((length (mOutput a)) /= 0) ) then a else (stepUntilOutput (step a))

type MachineArray = [MachineState]

steps :: Int -> Int -> MachineState -> MachineState
steps a b m = if (a == b) then m else (steps (a+1) b (step m))

main = do
     fHand <- openFile "data/Day9.txt" ReadMode
     contents <- hGetContents fHand
     let input = [(read x :: Integer) | x <- (splitOn ","  contents)]
     print (stepUntilTermination (mListToMemory input, 0, [2], [], 0))
     hClose fHand
