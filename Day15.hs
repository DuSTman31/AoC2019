import System.IO
import Data.List.Split
import Data.Maybe
import qualified Data.List as List
import Control.Exception
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set

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

type MachineArray = [MachineState]

steps :: Int -> Int -> MachineState -> MachineState
steps a b m = if (a == b) then m else (steps (a+1) b (step m))

type Picture = (Map.Map Coord Int, Int, Int, Int, Int)

pData :: Picture -> Map.Map Coord Int
pData (a, _, _, _, _) = a

pMinX :: Picture -> Int
pMinX (_, x, _, _, _) = x

pMinY :: Picture -> Int
pMinY (_, _, y, _, _) = y

pMaxX :: Picture -> Int
pMaxX (_, _, _, x, _) = x

pMaxY :: Picture -> Int
pMaxY (_, _, _, _, y) = y

pReplaceData :: Picture -> Map.Map Coord Int -> Picture
pReplaceData (a, b, c, d, e) n = (n, b, c, d, e)

pReplaceMinX :: Picture -> Int -> Picture
pReplaceMinX (a, b, c, d, e) n = (a, n, c, d, e)

pReplaceMinY :: Picture -> Int -> Picture
pReplaceMinY (a, b, c, d, e) n = (a, b, n, d, e)

pReplaceMaxX :: Picture -> Int -> Picture
pReplaceMaxX (a, b, c, d, e) n = (a, b, c, n, e)

pReplaceMaxY :: Picture -> Int -> Picture
pReplaceMaxY (a, b, c, d, e) n = (a, b, c, d, n)

type Coord = (Int, Int)
type Robot = (Picture, MachineState, Integer, Int)

rGetPic :: Robot -> Picture
rGetPic (x, _, _, _) = x

rReplacePic :: Robot -> Picture -> Robot
rReplacePic (_, d, a, b) e = (e, d, a, b)

rGetMachine :: Robot -> MachineState
rGetMachine (_, m, _, _) = m

rReplaceMachine :: Robot -> MachineState -> Robot
rReplaceMachine (a, _, c, d) m = (a, m, c, d)

rGetScore :: Robot -> Integer
rGetScore (_, _, s, _) = s

rReplaceScore :: Robot -> Integer -> Robot
rReplaceScore (a, b, c, d) s = (a, b, s, d)


enlarge :: Picture -> Coord -> Picture
enlarge p (cx, cy) = let minx = (\i -> if (cx < (pMinX i)) then (pReplaceMinX i cx) else i)
                         miny = (\i -> if (cy < (pMinY i)) then (pReplaceMinY i cy) else i)
                         maxx = (\i -> if (cx > (pMaxX i)) then (pReplaceMaxX i cx) else i)
                         maxy = (\i -> if (cy > (pMaxY i)) then (pReplaceMaxY i cy) else i)
                     in (maxy (maxx (miny (minx p))))


paint :: Picture -> Coord -> Int -> Picture
paint p (cx, cy) c = let np = enlarge p (cx, cy)
                     in pReplaceData np (Map.insert (cx, cy) c (pData np))

rPaint :: Robot -> Coord -> Int -> Robot
rPaint r c i = rReplacePic r (paint (rGetPic r) c i)

robotStep :: Robot -> Robot
robotStep r = let m1 = (stepUntilOutput (rGetMachine r))
                  x = mGetOutput m1
                  r1 = rReplaceMachine r (mConsumeOutput m1)
                  m2 = stepUntilOutput (rGetMachine r1)
                  y = mGetOutput m2
                  r2 = rReplaceMachine r1 (mConsumeOutput m2)
                  m3 = stepUntilOutput (rGetMachine r2)
                  tile = mGetOutput m3
                  r3 = rReplaceMachine r2 (mConsumeOutput m3) 
              in if (x == -1 && y == 0) then (rReplaceScore r3 tile) else (rPaint r3 ((fromIntegral x), (fromIntegral y)) (fromIntegral tile))

  
runRobotSteps :: Robot -> Robot
runRobotSteps r  = let mr = rGetMachine r
                       smr = stepUntilOutput mr
                   in if (mIsTerminated smr) then (rReplaceMachine r smr) else runRobotSteps (robotStep r)

type Route = (MachineState, Int, Coord)

routeMachine :: Route -> MachineState
routeMachine (m, _, _) = m

routeReplaceMachine :: Route -> MachineState -> Route
routeReplaceMachine (_, l, c) nm = (nm, l, c)

routeLength :: Route -> Int
routeLength (_, l, _) = l

routeIncLength :: Route -> Route
routeIncLength (m, l, c) = (m, (l+1), c)

routeCoord :: Route -> Coord
routeCoord (_, _, c) = c

routeReplaceCoord :: Route -> Coord -> Route
routeReplaceCoord (a, b, _) c = (a, b, c)

selectShortestRoute :: [Route] -> Route
selectShortestRoute r = foldl1 (\x y -> if (routeLength x < routeLength y) then x else y) r

routeExtendCoord :: Route -> Int -> Route
routeExtendCoord r d = let f = fst (routeCoord r)
                           s = snd (routeCoord r)
                       in case d of
                            1 -> routeReplaceCoord r (f, s-1)
                            2 -> routeReplaceCoord r (f, s+1)
                            3 -> routeReplaceCoord r (f-1, s)
                            4 -> routeReplaceCoord r (f+1, s)

routeMove :: Route -> Int -> Route
routeMove r d = routeExtendCoord (routeIncLength r) d

addRoutesToSet :: [Route] -> Set.Set Coord -> Set.Set Coord
addRoutesToSet r c = foldl (\x y -> Set.insert (routeCoord y) x) c r

dijkstra :: [Route] -> Set.Set Coord -> Route
dijkstra r cs = let sr = selectShortestRoute r
                    rr = filter (\x -> (x /= sr)) r
                    xr = (expandRouteIfNotInSet (routeReplaceMachine sr (mConsumeOutput (routeMachine sr))) cs)
                in if ((mGetOutput (routeMachine sr)) == 2) then (routeReplaceMachine sr (mConsumeOutput (routeMachine sr))) else (dijkstra (xr ++ rr) (addRoutesToSet xr cs))


expandRouteIfNotInSet :: Route -> Set.Set Coord -> [Route]
expandRouteIfNotInSet r cs = let mv = (\x -> routeMove (routeReplaceMachine r (stepUntilOutput (mAddInput (routeMachine r) x))) (fromIntegral x))
                             in filter (\x -> ((mGetOutput (routeMachine x)) /= 0) && (Set.notMember (routeCoord x) cs)) ([mv 1] ++ [mv 2] ++ [mv 3] ++ [mv 4])


depthSearch :: [Route] -> Set.Set Coord -> Set.Set Coord
depthSearch r c = let sr = selectShortestRoute r
                      rr = filter (\x -> x /= sr) r
                      xr = (expandRouteIfNotInSet (routeReplaceMachine sr (mConsumeOutput (routeMachine sr))) c)
                  in if (r == []) then c else depthSearch (xr ++ rr) (addRoutesToSet xr c)  

type Routes = (Map.Map Int [Route], Set.Set Coord)

routesDR :: Routes -> Map.Map Int [Route]
routesDR (r, _) = r

routesReplaceDR :: Routes -> Map.Map Int [Route] -> Routes
routesReplaceDR (_, b) dr = (dr, b)

routesVisitedSet :: Routes -> Set.Set Coord
routesVisitedSet (_, c) = c

routesReplaceVisitedSet :: Routes -> Set.Set Coord -> Routes
routesReplaceVisitedSet (a, _) ns = (a, ns)

routesShortestDist :: Routes -> Int
routesShortestDist r = if (Map.null (routesDR r)) then 0 else (head (Map.keys (routesDR r)))

selectShortestRoutes :: Routes -> [Route]
selectShortestRoutes r = let qr = Map.lookup (routesShortestDist r) (routesDR r)
                             in if (isNothing qr) then [] else (fromJust qr)

filterShortestRoutes :: Routes -> Routes
filterShortestRoutes r = let nr = (routesReplaceDR r (Map.delete (routesShortestDist r) (routesDR r)) )
                         in nr

expandRoutes :: [Route] -> Set.Set Coord -> [Route]
expandRoutes [] c = []
expandRoutes (h:t) c = let er = expandRouteIfNotInSet h c
                       in er ++ expandRoutes t (addRoutesToSet er c)

routesAddRoute :: Routes -> Route -> Routes
routesAddRoute r r1 = let rl = (routeLength r1)
                          ir = (Set.insert (routeCoord r1) (routesVisitedSet r))
                      in if (Set.member (routeCoord r1) (routesVisitedSet r)) then r else (if (isJust (Map.lookup rl (routesDR r))) then  (routesReplaceVisitedSet (routesReplaceDR r (Map.insert rl ((fromJust (Map.lookup rl (routesDR r))) ++ [r1]) (routesDR r))) ir) else (routesReplaceVisitedSet (routesReplaceDR r (Map.insert rl ([r1]) (routesDR r)))) ir)

routesAddRoutes :: Routes -> [Route] -> Routes
routesAddRoutes r [] = r
routesAddRoutes r (h:t) = routesAddRoutes (routesAddRoute r h) t

expansionPass :: Routes -> Set.Set Coord -> Int -> Int
expansionPass r c i = let xr = expandRoutes (selectShortestRoutes r) (routesVisitedSet r)
                          rar = routesAddRoutes (filterShortestRoutes r) (filter (\x -> (Set.member (routeCoord x) c)) xr)
                      in if (length xr == 0) then i else rar `seq` expansionPass rar c (i + 1)

expansionPass_i :: Routes -> Int -> Int -> Routes
expansionPass_i r i it = let xr = expandRoutes (selectShortestRoutes r) (routesVisitedSet r)
                             rar = routesAddRoutes (filterShortestRoutes r) xr
                         in if (i == it) then r else rar `seq` expansionPass_i rar (i + 1) it

main = do
     fHand <- openFile "data/Day15.txt" ReadMode
     contents <- hGetContents fHand
     let input = [(read x :: Integer) | x <- (splitOn ","  contents)]
         r = ((mListToMemory input, 0, [], [], 0), 0, (0,0))
         p1 = (dijkstra (expandRouteIfNotInSet r Set.empty) Set.empty)
         p2 = ((Map.insert (routeLength p1) ([p1]) Map.empty), (Set.insert (routeCoord p1) Set.empty))
     print p1
     print (expandRoutes (selectShortestRoutes p2) (routesVisitedSet p2))
--     print (filterShortestRoutes p2)
--     print (routesAddRoutes (filterShortestRoutes p2) (expandRoutes (selectShortestRoutes p2) (routesVisitedSet p2)) )
     print (expansionPass p2 (depthSearch (expandRouteIfNotInSet r Set.empty) Set.empty) 0)
     hClose fHand
