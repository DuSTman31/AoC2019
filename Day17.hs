import System.IO
import Data.List.Split
import Data.Maybe
import qualified Data.List as List
import Control.Exception
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

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

mIsWaitingForInput :: MachineState -> Bool
mIsWaitingForInput m = let gi = (\(_, _, _, x) -> x)
                           insn = gi (decodeInstruction (mGetOpcode m))
                       in if (insn == 3) then True else False

mHasOutput :: MachineState -> Bool
mHasOutput m = if ((length (mOutput m)) /= 0) then True else False

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
                         in if (mIsTerminated sa) then sa else sa `seq` stepUntilTermination sa

stepUntilOutput :: MachineState -> MachineState
stepUntilOutput a = if (mIsTerminated a || mHasOutput a) then a else (stepUntilOutput (step a))

stepUntilInput :: MachineState -> MachineState
stepUntilInput m = if (mIsTerminated m || mIsWaitingForInput m) then m else (stepUntilInput (step m))

stepUntilIO :: MachineState -> MachineState
stepUntilIO m = if (mIsTerminated m || mHasOutput m || mIsWaitingForInput m) then m else (stepUntilIO (step m))

-- Day 17 new

type Coord = (Int, Int)

cutIntoLines_i :: [Integer] -> [Integer] -> [[Integer]]
cutIntoLines_i [] r = [r]
cutIntoLines_i (h:t) r = if (h == 10) then [r] ++ (cutIntoLines_i t []) else (cutIntoLines_i t (r ++ [h]))

cutIntoLines :: [Integer] -> [[Integer]]
cutIntoLines r = cutIntoLines_i r []

lineToSet :: [Integer] -> Coord -> Set.Set Coord -> Set.Set Coord
lineToSet [] _ sc = sc
lineToSet (h:t) (x,y) sc = if ((h == 35) || (h == 94)) then (lineToSet t ((x+1), y) (Set.insert (x,y) sc)) else (lineToSet t ((x+1), y) sc)

linesToSet_i :: [[Integer]] -> Coord -> Set.Set Coord -> Set.Set Coord
linesToSet_i [] _ sc = sc
linesToSet_i (h:t) (_,y) sc = linesToSet_i t (0, (y+1)) (lineToSet h (0,y) sc)

linesToSet :: [[Integer]] -> Set.Set Coord
linesToSet r = linesToSet_i r (0,0) (Set.empty)

noAdjacentSquares :: Coord -> Set.Set Coord -> Int
noAdjacentSquares (x, y) sc = let check = (\x1 y1 -> if (Set.member (x1, y1) sc) then 1 else 0)
                              in (check (x+1) y) + (check x (y+1)) + (check (x-1) y) + (check x (y-1))

intersections :: Set.Set Coord -> [Coord]
intersections sc = filter (\n -> if ((noAdjacentSquares n sc) > 2) then True else False) (Set.toList sc)

itoc :: Integer -> String
itoc 35 = "#"
itoc 46 = "."
itoc 60 = "<"
itoc 62 = ">"
itoc 118 = "v"
itoc 94 = "^"

lineToString :: [Integer] -> String
lineToString [] = ""
lineToString (h:t) = itoc h ++ lineToString t


--- Functions to map out the route through the scaffold

--- distInLine - return how far you can travel in the current (d) direction.
distInLine_i :: Coord -> Int -> Int -> Set.Set Coord -> Int
distInLine_i (x, y) 0 d cs = if (Set.member (x, (y-1)) cs) then (distInLine_i (x, (y-1)) 0 (d+1) cs) else d
distInLine_i (x, y) 1 d cs = if (Set.member ((x+1), y) cs) then (distInLine_i ((x+1), y) 1 (d+1) cs) else d
distInLine_i (x, y) 2 d cs = if (Set.member (x, (y+1)) cs) then (distInLine_i (x, (y+1)) 2 (d+1) cs) else d
distInLine_i (x, y) 3 d cs = if (Set.member ((x-1), y) cs) then (distInLine_i ((x-1), y) 3 (d+1) cs) else d

distInLine :: Coord -> Int -> Set.Set Coord -> Int
distInLine c d cs = distInLine_i c d 0 cs

--- forwards - moves the coordinate forward x units in a given direction.
forwards :: Coord -> Int -> Int -> Coord
forwards (x, y) 0 dist = (x, (y-dist))
forwards (x, y) 1 dist = ((x+dist), y)
forwards (x, y) 2 dist = (x, (y+dist))
forwards (x, y) 3 dist = ((x-dist), y)

-- dirToTurn - return the direction that you can now turn that doesn't double back on yourself.
dirToTurn :: Coord -> Int -> Set.Set Coord -> Int
dirToTurn (x, y) 0 cs = if (Set.member ((x+1), y) cs) then 1 else (if (Set.member ((x-1), y) cs) then 3 else 4)
dirToTurn (x, y) 1 cs = if (Set.member (x, (y+1)) cs) then 2 else (if (Set.member (x, (y-1)) cs) then 0 else 4)
dirToTurn (x, y) 2 cs = if (Set.member ((x-1), y) cs) then 3 else (if (Set.member ((x+1), y) cs) then 1 else 4)
dirToTurn (x, y) 3 cs = if (Set.member (x, (y-1)) cs) then 0 else (if (Set.member (x, (y+1)) cs) then 2 else 4)

dirRelation :: Int -> Int -> Char
dirRelation 0 1 = 'R'
dirRelation 0 3 = 'L'
dirRelation 1 2 = 'R'
dirRelation 1 0 = 'L'
dirRelation 2 3 = 'R'
dirRelation 2 1 = 'L'
dirRelation 3 0 = 'R'
dirRelation 3 2 = 'L'

type MovIns = (Char, Int)

doRoute :: Coord -> Int -> Set.Set Coord -> [MovIns]
doRoute c d cs = let dtt = dirToTurn c d cs
                     dil = distInLine c dtt cs
                     ti = dirRelation d dtt
                 in if (dtt == 4) then [] else ((ti, dil):(doRoute (forwards c dtt dil) dtt cs))

robotInRow :: [Integer] -> Bool
robotInRow [] = False
robotInRow (h:t) = if (h == 94) then True else robotInRow t

robotX_i :: [Integer] -> Int -> Int
robotX_i (h:t) x = if (h == 94) then x else (robotX_i t (x+1))

robotX :: [Integer] -> Int
robotX r = robotX_i r 0

robotLocation_i :: [[Integer]] -> Int -> Coord
robotLocation_i (h:t) y = if (robotInRow h) then (robotX h, y) else (robotLocation_i t (y+1))

robotLocation :: [[Integer]] -> Coord
robotLocation r = robotLocation_i r 0

isRepeated_i :: [MovIns] -> [MovIns] -> Bool
isRepeated_i [] _ = True
isRepeated_i n [] = False
isRepeated_i (nh:nt) (hh:ht) = if (nh == hh) then (isRepeated_i nt ht) else False

isRepeated :: [MovIns] -> [MovIns] -> Bool
isRepeated n [] = False
isRepeated (nh:nt) (hh:ht) = if (nh == hh) then (if (isRepeated_i (nh:nt) (hh:ht)) then True else (isRepeated (nh:nt) ht)) else (isRepeated (nh:nt) ht)

findFirstRepeatedSeq_i :: [MovIns] -> [MovIns] -> [MovIns]
findFirstRepeatedSeq_i stt (hh:ht) = if (isRepeated (stt ++ [hh]) ht) then (findFirstRepeatedSeq_i (stt ++ [hh]) ht) else stt

findFirstRepeatedSeq :: [MovIns] -> [MovIns]
findFirstRepeatedSeq r = findFirstRepeatedSeq_i [] r

findLastRepeatedSeq :: [MovIns] -> [MovIns]
findLastRepeatedSeq r = reverse (findFirstRepeatedSeq (reverse r))

firstElements :: [MovIns] -> [MovIns] -> Bool
firstElements [] h = True
firstElements n [] = False
firstElements (nh:nt) (hh:ht) = if (nh == hh) then (firstElements nt ht) else False

matchLists :: [MovIns] -> [MovIns] -> [MovIns] -> [MovIns] -> Maybe (Char, [MovIns])
matchLists a b c r = let fe = (\x -> firstElements x r)
                         j = (\x y -> Just (x, drop (length y) r))
                     in if (fe a) then (j 'A' a) else (if (fe b) then (j 'B' b) else (if (fe c) then (j 'C' c) else Nothing))


fitRouteToComponents_i :: [MovIns] -> [MovIns] -> [MovIns] -> [MovIns] -> [Char] -> Maybe [Char]
fitRouteToComponents_i a b c r t = let ml = matchLists a b c r
                                       xcl = t ++ [fst (fromJust ml)]
                                   in if (isJust (ml)) then (if ((snd (fromJust ml)) == []) then (Just xcl) else (fitRouteToComponents_i a b c (snd (fromJust ml)) xcl)) else Nothing

fitRouteToComponents :: [MovIns] -> [MovIns] -> [MovIns] -> [MovIns] -> Maybe [Char]
fitRouteToComponents a b c r = fitRouteToComponents_i a b c r []


miSize :: MovIns -> Int
miSize (d, l) = if (l >=10) then 4 else 3

estimateSize :: [MovIns] -> Int
estimateSize r = (foldl1 (+) (map miSize r)) + ((length r) - 1)

-- fitOne - Taking a and b as fixed, can we choose  c so as to describe the path r?
fitOne_i :: [MovIns] -> [MovIns] -> [MovIns] -> [MovIns] -> Maybe [[MovIns]]
fitOne_i (h:t) a b [] = fitOne_i t a b [h]
fitOne_i (h:t) a b c = if ((estimateSize c) > 20) then Nothing else (if (isJust (fitRouteToComponents a b c (h:t) )) then (Just [a, b, c]) else (fitOne_i t a b (c ++ [h])))

fitOne :: [MovIns] -> [MovIns] -> [MovIns] -> Maybe [[MovIns]]
fitOne r a b = fitOne_i r a b [] 

repeatedAB :: [MovIns] -> [MovIns] -> [MovIns] -> Maybe [[MovIns]]
repeatedAB r a b = let sc = (\x -> (drop (length x) r))
                   in if (firstElements a r) then (
                        if (sc a == [])
                        then Nothing
                        else (repeatedAB (sc a) a b))
                      else (
                        if (firstElements b r)
                        then (
                          if (sc b == [])
                          then Nothing
                          else (repeatedAB (sc b) a b))
                        else (fitOne r a b))

-- fitTwo - Taking a as fixed, can we choose b and c so as to describe the path r?
fitTwo_i :: [MovIns] -> [MovIns] -> [MovIns] -> Maybe [[MovIns]]
fitTwo_i (h:t) a [] = fitTwo_i t a [h]
fitTwo_i (h:t) a b = if ((estimateSize b) > 20) then Nothing else (if (isJust (repeatedAB (h:t) a b)) then (repeatedAB (h:t) a b) else (fitTwo_i t a (b ++ [h])))

fitTwo :: [MovIns] -> [MovIns] -> Maybe [[MovIns]]
fitTwo r a = fitTwo_i r a []

repeatedA :: [MovIns] -> [MovIns] -> Maybe[[MovIns]]
repeatedA r a = if (firstElements a r) then (if ((drop (length a) r) == []) then Nothing else (repeatedA (drop (length a) r) a)) else (fitTwo r a)

fitThree_i :: [MovIns] -> [MovIns] -> Maybe [[MovIns]]
fitThree_i (h:t) [] = fitThree_i t [h]
fitThree_i (h:t) a = if ((estimateSize a) > 20) then Nothing else (if (isJust (repeatedA (h:t) a)) then (repeatedA (h:t) a) else (fitThree_i t (a ++ [h])))

fitThree :: [MovIns] -> Maybe [[MovIns]]
fitThree r = fitThree_i r []

partOne :: MachineState -> String
partOne m = let s = (linesToSet (cutIntoLines (mOutput (stepUntilTermination m))))
            in show (foldl1 (\x y -> x + y) (map (\(x,y) -> (x*y)) (intersections s)))

commaList :: [String] -> String
commaList [] = "\n"
commaList (h:[]) = h ++ "\n"
commaList (h:t) = h ++ "," ++ (commaList t)

compToString :: [MovIns] -> String
compToString mi = commaList (map (\x -> [(fst x)] ++ "," ++ (show (snd x))) mi)


partTwo :: MachineState -> String
partTwo m = let s = (linesToSet (cutIntoLines (mOutput (stepUntilTermination m))))
                rt = (doRoute (robotLocation (cutIntoLines (mOutput (stepUntilTermination m)))) 0 s)
                components = fromJust (fitThree rt)
                frc = fitRouteToComponents (components !! 0) (components !! 1) (components !! 2) rt
                ip = commaList (map (\x->[x]) (fromJust frc)) ++ compToString (components !! 0) ++ compToString (components !! 1) ++ compToString (components !! 2) ++ "n\n"
            in (show (stepUntilTermination (mReplaceInput (map (\x-> fromIntegral (Char.ord x)) ip) (mReplaceMem m (mMemPrimWrite 2 0 (mMemory m))))))

main = do
     fHand <- openFile "data/Day17.txt" ReadMode
     contents <- hGetContents fHand
     let input = [(read x :: Integer) | x <- (splitOn ","  contents)]
         m = (mListToMemory input, 0, [], [], 0)
         s = (linesToSet (cutIntoLines (mOutput (stepUntilTermination m))))
         rt = (doRoute (robotLocation (cutIntoLines (mOutput (stepUntilTermination m)))) 0 s)
     putStrLn (foldl1 (\x y -> x ++ "\n" ++ y) [lineToString x | x <- (cutIntoLines (mOutput (stepUntilTermination m)))])
     print "Part 1"
     print (partOne m)
     print "Part 2"
     print (robotLocation (cutIntoLines (mOutput (stepUntilTermination m))))
     print rt
     print (partTwo m)
--     print (compToString ((fromJust (fitThree rt)) !! 0))
--     print (commaList (map (\x -> [x]) (fromJust (fitRouteToComponents ((fromJust (fitThree rt)) !! 0) ((fromJust (fitThree rt)) !! 1) ((fromJust (fitThree rt)) !! 2) rt))))
--     print (intercalate ',' (fitRouteToComponents ((fromJust (fitThree rt)) !! 0) ((fromJust (fitThree rt)) !! 1) ((fromJust (fitThree rt)) !! 2) rt))
--     print (mGetOutput (stepUntilOutput (mReplaceMem m (mMemPrimWrite 2 0 m))))
     hClose fHand


