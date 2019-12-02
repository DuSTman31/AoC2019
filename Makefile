

all: Day1 Day1Big Day2

Day1: Day1.hs
	ghc Day1.hs

Day1Big: Day1.big.hs
	ghc Day1.big.hs -O2

Day2: Day2.hs
	ghc Day2.hs

