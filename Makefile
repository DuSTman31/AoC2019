

all: Day1 Day1Big Day2 Day3 Day4 Day5 Day6

Day1: Day1.hs
	ghc Day1.hs

Day1Big: Day1.big.hs
	ghc Day1.big.hs -O2

Day2: Day2.hs
	ghc Day2.hs

Day3: Day3.hs
	ghc Day3.hs -O2

Day4: Day4.hs
	ghc Day4.hs -O2

Day5: Day5.hs
	ghc Day5.hs

Day6: Day6.hs
	ghc Day6.hs -O2
