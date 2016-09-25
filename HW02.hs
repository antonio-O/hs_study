{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys)
	| x==y = 1 + exactMatches xs ys
	| otherwise = exactMatches xs ys



-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\x -> length $ filter (==x) xs) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ map (\(x, y)->min x y) (zip as bs)
	where 
		as = countColors xs
		bs = countColors ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move	
getMove secret guess = Move (guess) (exactMatch) (match - exactMatch)
	where
		exactMatch = exactMatches secret guess
		match = matches secret guess
-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess e n) secret = (getMove secret guess) == (Move guess e n)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move candidates = filter (isConsistent move) candidates

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = concatMap(\x -> map(\c->(c:x)) colors) (allCodes (n-1))

-- Exercise 7 -----------------------------------------
solve :: Code -> [Move]
solve secret = 	getMoves allcodes
	where
		allcodes = allCodes (length secret)
		getMoves :: [Code] -> [Move]
		getMoves [] = []
		getMoves (x:xs) = (getMove secret x):(getMoves (filterCodes (getMove secret x) xs))

-- Bonus ----------------------------------------------
fiveGuess :: Code -> [Move]
fiveGuess = undefined
