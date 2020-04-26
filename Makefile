

all: Day1 Day1Big Day2 Day3 Day4 Day5 Day6 Day7 Day8 Day9 Day10 Day11 Day12 Day13 Day15 Day17 Day19

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

Day7: Day7.hs
	ghc Day7.hs -O2

Day8: Day8.hs
	ghc Day8.hs -O2

Day9: Day9.hs
	ghc Day9.hs -O2

Day10: Day10.hs
	ghc Day10.hs -O2

Day11: Day11.hs
	ghc Day11.hs -O2

Day12: Day12.hs
	ghc Day12.hs -O2

Day13: Day13.hs
	ghc Day13.hs -O2

Day15: Day15.hs
	ghc Day15.hs -O2

Day17: Day17.hs
	ghc Day17.hs -O2

Day19: Day19.hs
	ghc Day19.hs -O2
