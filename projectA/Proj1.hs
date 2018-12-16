-- Author : Takuhiro Kikuchi
-- Student ID : 900550


{- 
   The main purpose of this program is to determine 
   which suspects to include in each lineup,
   and it aims to find the true culprits in the minimum number of guesses.
-}


-- Module Declaration
module Proj1 (Person, parsePerson, height, hair, sex,
              GameState, initialGuess, nextGuess, feedback) where


-- Libraries used in this module
import Data.List
import Data.Maybe (catMaybes)


-- Game State type
-- Used for storing previous guesses
type GameState = [[Person]]


-- Person type
-- Used for representing suspects
data Person = Person Char Char Char deriving (Eq, Show)


-- Feedback type
-- Used for representing the feedback
type Score = (Int, Int, Int, Int)


-- Takes a string whose each character represents
-- a suspect's characteristic, and returns a Maybe Person type
-- If invalid returns Nothing
parsePerson :: String -> Maybe Person
parsePerson [] = Nothing
parsePerson xs = if (length xs == 3 && validityCheck xs)
	                then do
	                	Just (Person (xs!!0) (xs!!1) (xs!!2))
	                else do
	                	Nothing


-- Takes a 3 character string whose each character represents
-- a suspect's characteristic, and returns a boolean value
-- if it is in the valid format
validityCheck :: String -> Bool
validityCheck [] = False
validityCheck xs = if ((xs!!0 `elem` "TS") && (xs!!1 `elem` "BRD") && (xs!!2 `elem` "MF"))
					  then do 
					  	True
					  else do 
					  	False


-- Produces all possible suspect pairs
-- Takes three lists of strings as input
-- Returns a list of person types
personCombinations :: [String] -> [String] -> [String] -> [Person]
personCombinations xs ys zs = catMaybes (map parsePerson [x ++ y ++ z | x <- xs, y <- ys, z <- zs])


-- Inspiration taken from http://mfukar.github.io/2015/07/26/haskell3.html
-- Takes an integer n and a list of Person types as input
-- Returns a GameState type
-- If empty then return a list of an empty list
-- If n equals 0, also a list of an empty list
combinations :: Int -> [Person] -> GameState
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n (x:xs) = headList ++ tailList
    where headList = [x : remainingElm | remainingElm <- combinations (n-1) xs]
          tailList
              | n <= length xs = combinations n xs
              | otherwise = []


-- Takes a Person type
-- Returns the height of the Person type
height :: Person -> Char
height (Person h _ _) = h


-- Takes a Person type
-- Returns the hair colour of the Person type
hair :: Person -> Char
hair (Person _ h _) = h


-- Takes a Person type
-- Returns the gender of the Person type
sex :: Person -> Char
sex (Person _ _ s) = s


-- First Guess
-- This was found through testing
-- Takes no input and returns a pair of Person types
firstGuess :: [Person]
firstGuess = [Person 'T' 'B' 'F', Person 'T' 'R' 'F']


-- Constant representing the number of persons in each lineup
_NUMPERSON = 2


-- Takes no input, and returns a pair of Person types and game state
-- GameState initially contains all possible combinations
initialGuess :: ([Person],GameState)
initialGuess = (firstGuess, combinations _NUMPERSON personTypes)
	where personTypes = personCombinations heightTypes hairTypes genderTypes
	      heightTypes = ["S", "T"]
	      hairTypes = ["B", "R", "D"]
	      genderTypes = ["M", "F"]


-- Takes a pair of the previous guess and gamestate as input, 
-- and the feedback to this guess
-- Returns a pair of the next guess and updated game state
nextGuess :: ([Person],GameState) -> Score -> ([Person],GameState)
nextGuess (oldGuess, oldGameState) oldFeedback = (newGuess, newGameState)
	where newGuess = predict newGameState
	      newGameState = optimise oldGameState oldGuess oldFeedback


-- Constant representing the number of distinct lineups
_LINEUPS = 33


-- Takes a game state
-- Returns the best guess from the GameState type
-- by foldr-ing the remaining guesses and finding the most optimal guess
predict :: GameState -> [Person]
predict gameState = snd (foldr determineMax result gameState)
    where result = (_LINEUPS, [])
          determineMax guess result
              | fst result > bestLength = (bestLength, guess)
              | otherwise = result
              where bestLength = maximum lengthList
                    groupedGuess = group (sort xs)
                    xs = [feedback guess x | x <- gameState]
                    lengthList = [(length x) | x <- groupedGuess]


-- Takes a GameState type, pair of Person types, and Score type as input
-- Returns an updated version of the GameState type
-- Compares the new feedback and old feedback,
-- and it determines whether to remove the guess from the GameState type.
optimise :: GameState -> [Person] -> Score -> GameState
optimise [] _ _ = []
optimise _ [] _ = []
optimise (x:xs) ys oldFeedback
    | oldFeedback == feedback x ys = x : optimise xs ys oldFeedback
    | otherwise = optimise xs ys oldFeedback


-- Constant representing the maximum number of distinct persons in
-- the combined list of true culprits and guess
_MAXUNIQUE = 4


-- Takes true culprits and a guess
-- Returns the correct answer to the guess
feedback :: [Person] -> [Person] -> Score
feedback culprits guess = (suspectMatch, heightMatch, hairMatch, genderMatch)
	where combinedList   = culprits ++ guess
	      suspectMatch   = _MAXUNIQUE - (uniqueLength combinedList)
	      distinctGuess = remove culprits guess suspectMatch
	      removedGuess = nub (guess ++ distinctGuess)
	      distinctCulprits = remove removedGuess culprits suspectMatch
	      heightMatch    = (numOccur (map height distinctCulprits) (map height distinctGuess))
	      hairMatch      = (numOccur (map hair distinctCulprits) (map hair distinctGuess))
	      genderMatch    = (numOccur (map sex distinctCulprits) (map hair distinctGuess))


-- Takes two lists of eq elements and the number of identified culprits 
-- If there're 2 identified culprits, returns an empty list
-- If 1, find it and remove it in the second list and return it
-- If 0, return the original second list
remove :: Eq a => [a] -> [a] -> Int -> [a]
remove _ _ 2 = []
remove _ ys 0 = ys
remove _ [] _ = []
remove [] ys _ = ys
remove xs ys 1
	| xs!!0 == ys!!0 = [ys!!1]
	| xs!!1 == ys!!0 = [ys!!1]
	| xs!!0 == ys!!1 = [ys!!0]
	| xs!!1 == ys!!1 = [ys!!0]


-- Takes a list of eq elements
-- Return the number of unique elements in the list
uniqueLength :: Eq a => [a] -> Int
uniqueLength [] = 0
uniqueLength xs = length (nub xs)


-- Takes two lists of eq elements
-- Returns the number of elements in one list that also occurs in the other
-- list
numOccur :: Eq a => [a] -> [a] -> Int
numOccur [] [] = 0
numOccur [] _ = 0
numOccur _ [] = 0
numOccur (x:xs) ys
	| x `elem` ys = 1 + numOccur xs ys
	| otherwise = numOccur xs ys
