import System.IO
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

skipOver :: (String, Int) -> Char -> (String, Int)
skipOver (a, 0) c = (a, 0)
skipOver (a, b) c = if ((head a) == c) then (skipOver ((tail a), (b-1)) c) else (a,b)

doubles :: String -> Int -> Int
doubles a 2 = if (a !! 0 == a !! 1) then 1 else 0
doubles a d = if (a !! 0 == a !! 1) then ((doubles (tail a) (d-1)) + 1) else (doubles (tail a) (d -1))

doubleButNotTriple :: String -> Int -> Int
doubleButNotTriple a 0 = 0
doubleButNotTriple a 1 = 0
doubleButNotTriple a 2 = if ((a !! 0) == (a !! 1)) then 1 else 0
doubleButNotTriple a d = let m = (skipOver (a, d) (head a))
		   	     p = (doubleButNotTriple (fst m) (snd m))
		         in if ((a !! 0) == (a !! 1)) then (if (a !! 0 /= a !! 2) then (p + 1) else p) else (doubleButNotTriple (tail a) (d - 1))

filterDoubles :: Int -> Bool
filterDoubles a = if ((doubles (show a) (length (show a))) > 0) then True else False

noLower :: Int -> [Int] -> Bool
noLower a [] = True
noLower a b = if ((head b) < a) then False else (noLower a (tail b))

noLowerSeq :: [Int] -> Bool
noLowerSeq [] = True
noLowerSeq a = if (noLower (head a) (tail a)) then (noLowerSeq (tail a)) else False

toDigits :: Int -> [Int]
toDigits a = if (a == 0) then [] else ((toDigits (div a 10)) ++ [mod a 10])

isSequential :: Int -> Bool
isSequential a = noLowerSeq (toDigits a)

dualFilter :: Int -> Bool
dualFilter a = (filterDoubles a) && (isSequential a)

filterDBNT :: Int -> Bool
filterDBNT a = if ((doubleButNotTriple (show a) (length (show a))) > 0) then True else False

tripleFilter :: Int -> Bool
tripleFilter a = (isSequential a) && (filterDBNT a)

main = do
     print (length (filter dualFilter [248345..746315]))
--     print (doubleButNotTriple "aabjkliu" 8)
     print (length (filter tripleFilter [248345..746315]))

