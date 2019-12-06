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

mAdvPos :: Int -> MachineState -> MachineState
mAdvPos e (a, b, c, d) = (a, (b+e), c, d)

mGetOpcode :: MachineState -> Int
mGetOpcode a = mGetRelMemory 0 a

doubleIndirect :: [Int] -> Int -> Int
doubleIndirect a b = a !! (a !! b)


addStep :: MachineState -> MachineState
addStep a = mAdvPos 4 (mSetIndMemory 3 ((mGetIndMemory 1 a) + (mGetIndMemory 2 a)) a)

multStep :: MachineState -> MachineState
multStep a = mAdvPos 4 (mSetIndMemory 3 ((doubleIndirect (mMemory a) ((mPos a)+1)) * (doubleIndirect (mMemory a) ((mPos a)+2))) a)

inputStep :: MachineState -> MachineState
inputStep a = a

outputStep :: MachineState -> MachineState
outputStep a = a

executeOpcode :: Int -> MachineState -> MachineState
executeOpcode 1 b = addStep b
executeOpcode 2 b = multStep b
--executeOpcode 3 b = inputStep b
--executeopcode 4 b = outputStep b

doSteps :: MachineState -> MachineState
doSteps a = if (mGetOpcode a == 99) then a else (doSteps (executeOpcode (mGetOpcode a) a))


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
     fHand <- openFile "data/Day2.txt" ReadMode
     contents <- hGetContents fHand
     let input = [(read x :: Int) | x <- (splitOn ","  contents)]
     print input
     print (mGetIndMemory 1 ([2, 0, 0, 0, 1, 0, 0, 0, 99], 4, [], []))
--     print (doSteps ([1,0,0,0,1,0,0,0,99], 0, [], []))
     print (doSteps (replaceParams (input, 0, [], []) 12 2))
--     pout (attemptSeq2 (input, 0, [], []) 0)
     hClose fHand
