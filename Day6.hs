import System.IO
import Data.List.Split
import Data.Maybe

filterOrbiter :: String -> [String] -> Bool
filterOrbiter a b = if ((b !! 0) == a) then True else False

orbiters :: String -> [[String]] -> [[String]]
orbiters a b = filter (filterOrbiter a) b

-- Part 1 top-level function.
sumAllOrbits :: String -> Int -> [[String]] -> Int
sumAllOrbits a b c = (sum [sumAllOrbits (x !! 1) (b+1) c | x <- (orbiters a c)]) + b

bFold :: Bool -> Bool -> Bool
bFold a b = if (a == True) then True else (if (b == True) then True else False)

inSubtree :: String -> String -> [[String]] -> Bool
inSubtree cur needle d = if (cur == needle) then True else (foldl (||) False [inSubtree (x !! 1) needle d | x <- (orbiters cur d)])

tryPathCandidates :: String -> String -> [[String]] -> [[String]] -> [String]
tryPathCandidates cur needle [] _ = if (cur == needle) then [cur] else []
tryPathCandidates cur needle d full = if (inSubtree ((head d) !! 1) needle full) then ([cur] ++ (pathToObj ((head d) !! 1) needle full)) else (tryPathCandidates cur needle (tail d) full)

pathToObj :: String -> String -> [[String]] -> [String]
pathToObj cur needle d = tryPathCandidates cur needle (orbiters cur d) d

discardCommonHead :: ([String], [String]) -> ([String], [String])
discardCommonHead (a, b) = if ((head a) == (head b)) then (discardCommonHead ((tail a), (tail b))) else (a,b )

getDist:: ([String], [String]) -> Int
getDist (a, b) = ((length a) -1) + ((length b) -1) 

main = do
     fHand <- openFile "data/Day6.txt" ReadMode
     contents <- hGetContents fHand
     let input = [splitOn ")" y | y <- lines contents]
     print (sumAllOrbits "COM" 0 input)
     print (getDist (discardCommonHead ((pathToObj "COM" "SAN" input), (pathToObj "COM" "YOU" input))))

     hClose fHand

