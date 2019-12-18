import System.IO
import Data.Char
import Data.Maybe
import Data.List.Split
import Data.List
import Debug.Trace

type Coord = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Moon = (Coord, Velocity)

getCoord :: Moon -> Coord
getCoord (a, b) = a

getVelocity :: Moon -> Velocity
getVelocity (a, b) = b

replaceCoord :: Moon -> Coord -> Moon
replaceCoord a b = (b, getVelocity a)

replaceVelocity :: Moon -> Velocity -> Moon
replaceVelocity a b = (getCoord a,  b)

vGetX :: Velocity -> Int
vGetX (x, y, z) = x

cGetX :: Coord -> Int
cGetX (x, y, z) = x

vGetY :: Velocity -> Int
vGetY (x, y, z) = y

cGetY :: Coord -> Int
cGetY (x, y, z) = y

vGetZ :: Velocity -> Int
vGetZ (x, y, z) = z

cGetZ :: Coord -> Int
cGetZ (x, y, z) = z


-- Parsing functions 
matchChar :: Char -> String -> Maybe String
matchChar a s = if ((head s) == a) then (Just (tail s)) else Nothing

parseN_int :: String -> Bool -> Int -> Maybe (Int, String)
parseN_int [] t a= if t then (Just (a, [])) else Nothing
parseN_int s t a = if (isDigit (head s)) then (parseN_int (tail s) True ((a * 10) + (read [(head s)] :: Int))) else (if t then (Just (a, s)) else Nothing )

parseNum :: String -> Maybe (Int, String)
parseNum [] = Nothing
parseNum a = if ((head a) == '-') then (if (isJust (parseN_int (tail a) False 0)) then let sub = (parseN_int (tail a) False 0)
                                                                                           in (Just (((fst (fromJust sub))* (-1)), (snd (fromJust sub)))) else Nothing) else (parseN_int a False 0) 

parseField :: String -> Maybe (Int, String)
parseField [] = Nothing
parseField a = if (isAlphaNum (head a)) then (parseField (tail a)) else (if ((head a)=='=') then (parseNum (tail a)) else Nothing)

removeBrackets_tail :: String -> String
removeBrackets_tail a = if ((head a)=='>') then [] else ([head a] ++ (removeBrackets_tail (tail a)))

removeBrackets :: String -> String
removeBrackets a = if ((head a) == '<') then (removeBrackets_tail (tail a)) else a

removeLeadingSpaces :: String -> String
removeLeadingSpaces [] = []
removeLeadingSpaces a = if ((head a)==' ') then removeLeadingSpaces (tail a) else a 

allJust :: [Maybe a] -> Bool
allJust [] = True
allJust b = if (isNothing (head b)) then False else allJust (tail b)

parseLine :: String -> Maybe Coord
parseLine a = let op = (map (\x -> parseField x) (map removeLeadingSpaces (splitOn "," (removeBrackets a))))
              in if (allJust op) then (Just ((fst (fromJust (op !! 0))), (fst (fromJust (op !! 1))), (fst (fromJust (op !! 2))))) else Nothing


-- Simulation of motion
testOrd :: Int -> Int -> Int
testOrd a b = if (a == b) then 0 else (if (a > b) then 1 else (-1))

updateV :: Moon -> Moon -> Moon
updateV a b = replaceVelocity b (((vGetX (getVelocity b)) + (testOrd (cGetX (getCoord a)) (cGetX (getCoord b)))), ((vGetY (getVelocity b)) + (testOrd (cGetY (getCoord a)) (cGetY (getCoord b)))), ((vGetZ (getVelocity b)) + (testOrd (cGetZ (getCoord a)) (cGetZ (getCoord b)))))

updateVelocities :: [Moon] -> [Moon]
updateVelocities [] = []
updateVelocities a = [foldl1 (\x y -> updateV y x) a] ++ (updateVelocities (map (\x -> updateV (head a) x) (tail a)))

updatePosition :: [Moon] -> [Moon]
updatePosition a = map (\x -> replaceCoord x ((cGetX (getCoord x)) + (vGetX (getVelocity x)), (cGetY (getCoord x)) + (vGetY (getVelocity x)), (cGetZ (getCoord x)) + (vGetZ (getVelocity x)))) a

-- Perform a single cycle of the simulation
updateCycle :: [Moon] -> [Moon]
updateCycle a = (updatePosition (updateVelocities a))

-- Perform i cycles of the simulation
updateCycles :: [Moon] -> Int -> [Moon]
updateCycles a i = if (i == 0) then a else (updateCycles (updateCycle a) (i-1))


potentialEnergy  :: Moon -> Int
potentialEnergy a = abs (cGetX (getCoord a)) + abs (cGetY (getCoord a)) + abs (cGetZ (getCoord a))

kineticEnergy :: Moon -> Int
kineticEnergy a = abs (cGetX (getVelocity a)) + abs (cGetY (getVelocity a)) + abs (cGetZ (getVelocity a))

totalEnergy :: Moon -> Int
totalEnergy a = kineticEnergy a * potentialEnergy a


-- Isolate a specific axis from the cooordinates and velocity
isolateX :: Moon -> (Int, Int)
isolateX a = (cGetX (getCoord a), vGetX (getVelocity a))

isolateY :: Moon -> (Int, Int)
isolateY a = (cGetY (getCoord a), vGetY (getVelocity a))

isolateZ :: Moon -> (Int, Int)
isolateZ a = (cGetZ (getCoord a), vGetZ (getVelocity a))

simulateUntilRepeat_int_x ::  [Moon] -> [Moon] -> Integer -> Integer
simulateUntilRepeat_int_x a b i = let ucb = updateCycle b
                                      ui = i + 1
                                      f = (\y -> (simulateUntilRepeat_int_x a y ui))
                                  in if ((map isolateX a) == (map isolateX b)) then i else f ucb

simulateUntilRepeat_int_y :: [Moon] -> [Moon] -> Integer -> Integer
simulateUntilRepeat_int_y a b i = let ucb = updateCycle b
                                      ui = i + 1
                                      f = (\y -> (simulateUntilRepeat_int_y a y ui))
                                  in if ((map isolateY a) == (map isolateY b)) then i else f ucb

simulateUntilRepeat_int_z ::  [Moon] -> [Moon] -> Integer -> Integer
simulateUntilRepeat_int_z a b i = let ucb = updateCycle b
                                      ui = i + 1
                                      f = (\y -> (simulateUntilRepeat_int_z a y ui))
                                  in if ((map isolateZ a) == (map isolateZ b)) then i else f ucb
                                                                                

simulateUntilRepeat :: [Moon] -> (Integer, Integer, Integer)
simulateUntilRepeat a = (simulateUntilRepeat_int_x a (updateCycle a) 1, simulateUntilRepeat_int_y a (updateCycle a) 1, simulateUntilRepeat_int_z a (updateCycle a) 1)

main = do
     fHand <- openFile "data/Day12.txt" ReadMode
     contents <- (hGetContents fHand)
     let !ip = (map (\x -> (fromJust x, (0,0,0))) [parseLine x | x <- lines contents])
     print "Part 1: "
     print (sum (map totalEnergy (updateCycles ip 1000)))
     print "Part 2: "
     print (foldl1 lcm ((\(x, y, z) -> [x, y, z]) (simulateUntilRepeat ip)))
     hClose fHand
