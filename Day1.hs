import System.IO

fuelRequired :: Int -> Int
fuelRequired x = (div x  3) - 2

fuelRequiredR :: Int -> Int -> Int
fuelRequiredR x y = if (fuelRequired y < 0) then x else (fuelRequiredR (x + fuelRequired y) (fuelRequired y))

main = do
     fHand <- openFile "data/Day1.txt" ReadMode
     contents <- hGetContents fHand
     putStrLn (show (sum [fuelRequired (read x :: Int) | x <- lines contents]))
     putStrLn (show (sum [fuelRequiredR 0 (read x :: Int) | x <- lines contents]))     
     hClose fHand