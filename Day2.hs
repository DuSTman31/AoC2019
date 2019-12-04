import System.IO
import Data.List.Split
import Data.Maybe

replaceAtInt :: [a] -> Int -> Int -> a -> [a]
replaceAtInt a b c d = if (c == b) then ([d] ++ (tail a)) else ([head a] ++ (replaceAtInt (tail a) b (c + 1) d))

replaceAt :: [a] -> Int -> a -> [a]
replaceAt a b c = replaceAtInt a b 0 c

doubleIndirect :: [Int] -> Int -> Int
doubleIndirect a b = a !! (a !! b)

addStep :: ([Int], Int) -> ([Int], Int)
addStep (a, pos) = ((replaceAt a (a !! (pos+3)) ((doubleIndirect a (pos + 1)) + (doubleIndirect a (pos+2)))), (pos+4))

multStep :: ([Int], Int) -> ([Int], Int)
multStep (a, pos) = ((replaceAt a (a !! (pos+3)) ((doubleIndirect a (pos+1)) * (doubleIndirect a (pos+2)))), (pos+4))

doStep :: ([Int], Int) -> ([Int], Int)
doStep (a, pos) = if (a !! pos == 1) then (addStep (a, pos)) else (multStep (a, pos))

doSteps :: ([Int], Int) -> ([Int], Int)
doSteps (a, pos) = if (a !! pos == 99) then (a, pos) else (doSteps (doStep (a, pos)))


replaceParams :: [Int] -> Int -> Int -> [Int]
replaceParams a b c = (replaceAt (replaceAt a 2 c) 1 b)

-- Code for Part 2.

attempt :: ([Int], Int) -> Int -> Int -> Bool
attempt (a, b) c d = if ((fst (doSteps ((replaceParams a c d), 0))) !! 0 == 19690720) then True else False

attemptSeq :: ([Int], Int) -> Int -> Int -> Maybe (Int, Int)
attemptSeq (a, b) c d = if (d == 100) then Nothing else (if (attempt (a, b) c d) then Just (c, d) else (attemptSeq (a, b) c (d+1)))

attemptSeq2 :: ([Int], Int) -> Int -> Maybe (Int, Int)
attemptSeq2 (a, b) c = if (c == 100) then Nothing else (if ((isNothing (attemptSeq (a, b) c 0) /= True)) then (attemptSeq (a, b) c 0) else (attemptSeq2 (a, b) (c + 1)))

pout :: Maybe (Int, Int) -> IO ()
pout Nothing = putStrLn "Nothing"
pout (Just a) = do
     putStr "Just "
     putStrLn (show a)

main = do
     fHand <- openFile "data/Day2.txt" ReadMode
     contents <- hGetContents fHand
     let input = [(read x :: Int) | x <- (splitOn ","  contents)]
     print (doSteps ((replaceParams input 12 2), 0))
     pout (attemptSeq2 (input, 0) 0)
     hClose fHand

-- main = print (doSteps ([1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50], 0))
