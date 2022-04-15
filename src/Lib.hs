module Lib where

import Data.List.Split (splitOn)
import Data.List (nub)

data Result = Match | Misplaced | Miss deriving Eq
data UserAction = Results [Result] | Quit

parseInput :: String -> [String]
parseInput = splitOn "," . filter ('"' /=)

minimumBy :: Ord a => (a -> Int) -> [a] -> a
minimumBy _ [] = error "No remaining matching words. You probably made a mistake."
minimumBy f xs = snd <$> minimum $ map (\x -> (f x, x)) xs

solve :: [String] -> String
solve wordList = minimumBy (largestResultSet wordList) wordList

largestResultSet :: [String] -> String -> Int
largestResultSet wordList word = maximum $ map (length . filterResults wordList word) allResults

filterResults :: [String] -> String -> [Result] -> [String]
filterResults wordList word result = filter (isMatch word result) wordList

parseResult :: String -> Maybe UserAction
parseResult ('x':_) = Just Quit
parseResult xs = Results <$> allMaybes (map parseResultChar xs)

allMaybes :: [Maybe a] -> Maybe [a]
allMaybes [] = Just []
allMaybes (Nothing:_) = Nothing
allMaybes (Just x : xs) = (x :) <$> allMaybes xs

parseResultChar :: Char -> Maybe Result
parseResultChar 'g' = Just Match
parseResultChar 'b' = Just Miss
parseResultChar 'y' = Just Misplaced
parseResultChar _ = Nothing

resultList :: [Result]
resultList = [Match, Misplaced, Miss]

allResults :: [[Result]]
allResults = [[a,b,c,d,e] | a <- resultList, b <- resultList, c <- resultList, d <- resultList, e <- resultList]

isMatch :: String -> [Result] -> String -> Bool
isMatch guessWord result word = isMatchDirect guessWord result word && isMatchMisplaced guessWord result word

isMatchDirect :: String -> [Result] -> String -> Bool
isMatchDirect guess result word = and (zipWith (==) matches greens)
    where matches = zipWith (==) guess word
          greens = map (Match ==) result

isMatchMisplaced :: String -> [Result] -> String -> Bool
isMatchMisplaced guess result word = all (\c -> if blacksPresent c then freeCharCount c == yellowCount c else freeCharCount c >= yellowCount c) resultChars
    where guessResult = zip guess result
          freeChars = map fst $ filter ((Match /=) . snd) $ zip word result
          resultChars = nub $ map fst $ filter ((Match /=) . snd) guessResult
          yellowCount c = length $ filter ((c, Misplaced) ==) guessResult
          blacksPresent c = (c, Miss) `elem` guessResult
          freeCharCount c = length $ filter (c ==) freeChars