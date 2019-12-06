import System.IO
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

parseEntry :: String -> (Char, Int)
parseEntry a = (head a, (read (tail a) :: Int))

parseEntryList :: String -> [(Char, Int)]
parseEntryList a = [(parseEntry x) | x <- (splitOn "," a)]

-- Move takes a direction and a current set of coords
--  and returns the coords after moving one square in that
--  direction.
move :: Char -> (Int, Int) -> (Int, Int)
move 'R' (x, y) = ((x + 1), y)
move 'L' (x, y) = ((x - 1), y)
move 'U' (x, y) = (x, (y - 1))
move 'D' (x, y) = (x, (y + 1))

posAfter :: (Char, Int) -> (Int, Int) -> (Int, Int)
posAfter ('R', l) (x, y) = ((x + l), y)
posAfter ('L', l) (x, y) = ((x - l), y)
posAfter ('U', l) (x, y) = (x, (y - l))
posAfter ('D', l) (x, y) = (x, (y + l))

-- loggedMove - Will take a starting position (Int, Int) and a command
---  (Char, Int) and produce a list of all the square coords on that path
loggedMove :: ((Int, Int), Int) -> (Char, Int) -> [((Int, Int), Int)]
loggedMove ((x, y), td) (d, l) = if (l == 0) then [] else ([((x,y), td)] ++ (loggedMove ((move d (x,y)), (td + 1)) (d, (l - 1)) ))

-- loggedMoveSeq - Will execute loggedMove series of instructions
loggedMoveSeq :: [(Char, Int)] -> ((Int, Int), Int) -> [((Int, Int), Int)]
loggedMoveSeq [] ((b, c), d) = []
loggedMoveSeq a ((b, c), d) = let res = (loggedMove ((b, c), d) (head a))
	      in res ++ (loggedMoveSeq (tail a) ((posAfter (head a) (b, c)), (d + (abs (snd (head a))))))

-- We're supposed to not count (0,0) so make a function to filter it out.
filterZero :: (Int, Int) -> Bool
filterZero (0, 0) = False
filterZero (a, b) = True

overlappedSquares :: [((Int, Int), Int)] -> [((Int, Int), Int)] -> [(Int, Int)]
overlappedSquares a b = Set.toList (Set.intersection (Set.filter filterZero (Set.fromList (map fst b))) (Set.filter filterZero (Set.fromList (map fst a))))

manhattanDistance :: (Int,Int) -> Int
manhattanDistance (x,y) = (abs x) + (abs y)

lowerMember :: Int -> Int -> Maybe Int
lowerMember a b = if (a < b) then (Just a) else (Just b)

-- lToM - converts a list of coordinate and distances into a map,
--   with the values being the lower of any in for the coordinate.
lToM_int :: Map.Map (Int, Int) Int -> ((Int, Int), Int) -> Map.Map (Int, Int) Int
lToM_int b a = let c = (Map.lookup (fst a) b)
	 in if (isNothing c) then (Map.insert (fst a) (snd a) b) else (Map.update (lowerMember (snd a)) (fst a) b)

lToM :: [((Int, Int), Int)] -> Map.Map (Int, Int) Int
lToM [] = Map.empty
lToM a = lToM_int (lToM (tail a)) (head a)

part2 :: [((Int, Int), Int)] -> [((Int, Int), Int)] -> [(Int, Int)] -> Int
part2 a b c = (minimum [((fromJust (Map.lookup i (lToM a))) + (fromJust(Map.lookup i (lToM b)))) | i <- c])

main = do
     fHand <- openFile "data/Day3.txt" ReadMode
     contents <- hGetContents fHand
     let input = [parseEntryList y | y <- (lines contents)]
     print (minimum (map manhattanDistance  (overlappedSquares (loggedMoveSeq (head input) ((0,0),0)) (loggedMoveSeq (head (tail input)) ((0,0),0)))))
     print (part2 (loggedMoveSeq (head input) ((0,0),0)) (loggedMoveSeq (head (tail input)) ((0,0),0)) (overlappedSquares (loggedMoveSeq (head input) ((0,0),0)) (loggedMoveSeq (head (tail input)) ((0,0),0))))
     hClose fHand

