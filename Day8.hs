import System.IO
import Data.List.Split
import Data.Char

toDigits :: String -> [Int]
toDigits a = map (\x -> (read [x] :: Int)) a

countDig :: Int -> [Int] -> Int
countDig c a = (foldl (\z x -> if (x == c) then (z + 1) else z) 0 a)

ff :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
ff (a, b, c) (d, e, f) = if (a < d) then (a, b, c) else (d,e,f)

partOne :: (Int, Int, Int) -> Int
partOne (a, b, c) = b * c

pixelFilter :: Int -> Int -> Int
pixelFilter a b = (if (b == 2) then a else b)

photoFilter :: [Int] -> [Int] -> [Int]
photoFilter a b = zipWith pixelFilter a b

emptyList :: Int -> [Int]
emptyList a = if (a ==0) then [] else ([0] ++ (emptyList (a - 1)))

main = do
     fHand <- openFile "data/Day8.txt" ReadMode
     contents <- hGetContents fHand
     let layers = (map toDigits (chunksOf (25 * 6) (filter isDigit contents)))
     print (partOne (foldl ff (9999, 0, 0) [(countDig 0 x, countDig 1 x, countDig 2 x) | x <- layers]))
     print (foldl photoFilter (emptyList (25 * 6)) (reverse layers))
     hClose fHand