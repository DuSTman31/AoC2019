import System.IO
import qualified Data.Set as Set
import qualified Data.List as List

type Coord = (Int, Int)

inputFilter :: Char -> Bool
inputFilter '.' = True
inputFilter '#' = True
inputFilter _ = False

asteroidPresent :: Char -> Bool
asteroidPresent '#' = True
asteroidPresent '.' = False

asteroidListRow :: Int -> Int ->  [Bool] -> [Coord]
asteroidListRow x y [] = []
asteroidListRow x y z = let t = (asteroidListRow (x+1) y (tail z))
		in if (head z) then ([(x, y)] ++ t) else t

asteroidList :: Int ->[[Bool]] -> [Coord]
asteroidList y [] = []
asteroidList y z = (asteroidListRow 0 y (head z)) ++ (asteroidList (y+1) (tail z))

relativeTo :: Coord -> [Coord] -> [Coord]
relativeTo (ax, ay) b = map (\(x, y) -> ((x - ax), (y - ay))) b

relToAbs :: Coord -> [Coord] -> [Coord]
relToAbs (ax, ay) b = map (\(x, y) -> (x + ax, y + ay)) b

isolateColumn :: Int -> [Coord] -> [Coord]
isolateColumn a b = filter (\(x, y) -> if (x==a) then True else False) b

isolateRow :: Int -> [Coord] -> [Coord]
isolateRow a b = filter (\(x, y) -> if (y==a) then True else False) b

asteroidsInCardinalDirections :: Coord -> [Coord] -> [Coord]
asteroidsInCardinalDirections (ax, ay) b = (isolateColumn ax b) ++ (isolateRow ay b)

closestCardinalCoords :: [Coord] -> [Coord]
closestCardinalCoords a = let ip = (filter (\x -> (x /= (0,0))) (isolateColumn 0 a))
		      	      ip2 = (filter (\x -> (x /= (0,0))) (isolateRow 0 a))
		      in [(foldl1 (\(x1, y1) (x2, y2) -> if (y1 > y2) then (x1, y1) else (x2, y2)) (filter (\(x, y) -> if (y < 0) then True else False) ip))] ++ [(foldl1 (\(x1, y1) (x2, y2) -> if (y1 < y2) then (x1, y1) else (x2, y2)) (filter (\(x, y) -> if (y > 0) then True else False) ip))] ++ [(foldl1 (\(x1, y1) (x2, y2) -> if (x1 > x2) then (x1, y1) else (x2, y2)) (filter (\(x, y) -> if (x < 0) then True else False) ip2))] ++ [(foldl1 (\(x1, y1) (x2, y2) -> if (x1 < x2) then (x1, y1) else (x2, y2)) (filter (\(x, y) -> if (x > 0) then True else False) ip2))]


asteroidsDiagonally :: [Coord] -> [Coord]
asteroidsDiagonally b = filter (\(x, y) -> if ((x==0) || (y==0)) then False else True) b

isBlocked_R :: Int -> Coord -> Coord -> Set.Set Coord -> Bool
isBlocked_R n (gx, gy) (x,y) s = let ccx = (n*gx, n*gy)
                                 in if (ccx == (x,y)) then False else (if (Set.member ccx s) then True else (isBlocked_R (n+1) (gx, gy) (x,y) s))

isBlocked :: Coord -> Set.Set Coord -> Bool
isBlocked (x, y) s = let g = (gcd x y)
                         in isBlocked_R 1 ((div x g), (div y g)) (x,y) s

unBlockedDiagonalAsteroids :: [Coord] -> [Coord]
unBlockedDiagonalAsteroids a = let s = Set.fromList (asteroidsDiagonally a)
                                   in (filter (\x -> (not (isBlocked x s))) (asteroidsDiagonally a))

visibleAsteroids :: Coord -> [Coord] -> [Coord]
visibleAsteroids a b = let r = (relativeTo a b)
                           in (closestCardinalCoords r) ++ (unBlockedDiagonalAsteroids r)

getMaximallyVisibleAsteroid :: [Coord] -> (Coord, Int)
getMaximallyVisibleAsteroid a = foldl1 (\x y -> if ((snd x) > (snd y)) then x else y) [(x, (length (visibleAsteroids x a))) | x <- a]

coordToAngle :: Coord -> Coord -> Float
coordToAngle o p = atan2 ((fromIntegral (fst o)) - (fromIntegral (fst p))) ((fromIntegral (snd p)) - (fromIntegral (snd o))) 

sortByAngle :: Coord -> [Coord] -> [Coord]
sortByAngle a b = List.sortBy (\x y -> if ((coordToAngle a x)>(coordToAngle a y)) then GT else LT) b

getAsteroidsByAngle :: Coord -> [Coord] -> [Coord]
getAsteroidsByAngle r a = (sortByAngle r (relToAbs r (visibleAsteroids r a)))

main = do
     fHand <- openFile "data/Day10.txt" ReadMode
     contents <- hGetContents fHand
     let input = (map (\x -> (map asteroidPresent x)) [filter inputFilter x | x <- (lines contents)])
         al = (asteroidList 0 input)
     print (getMaximallyVisibleAsteroid al)
     print ((getAsteroidsByAngle (fst (getMaximallyVisibleAsteroid al)) al) !! 198) 
--     print (getMaximallyVisibleAsteroid al)
     hClose fHand
