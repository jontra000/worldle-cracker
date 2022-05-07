module Main where

import Lib (solve, solveHard, largestResultSet, parseInput, filterResults, parseResult, Result, UserAction (Quit, Results))
import qualified Data.Maybe
import System.Environment (getArgs)
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs
    input <- readFile "wordle.txt"
    let initWord = "arise"
        ws = parseInput input
        hardMode = "--hard" `elem` args
        solver = if hardMode then solveHard else solve ws
    next solver initWord ws

next :: ([String] -> String) -> String -> [String] -> IO ()
next solver guess wordList = do
    putStrLn guess
    result <- nextLine
    next' solver result guess wordList 

next' :: ([String] -> String) -> UserAction -> String -> [String] -> IO ()
next' _ Quit _ _ = return ()
next' solver (Results result) guess wordList =
    let wordList' = filterResults wordList guess result
        guess' = solver wordList'
    in  case wordList' of
            [] -> do
                putStrLn "No more possible matches. Perhaps you made a mistake?"
                return ()
            [x] -> do
                putStrLn x
                putStrLn "solved"
                return ()
            _ ->
                next solver guess' wordList'

nextLine :: IO UserAction
nextLine = retryIO readResult

retryIO :: IO (Maybe a) -> IO a
retryIO f = do
    r <- f
    case r of
        Nothing -> retryIO f
        Just x -> return x

readResult :: IO (Maybe UserAction)
readResult = do
    putStrLn "enter results (g=green, b=black/grey, y=yellow) e.g. 'ggbyb', or 'x' to quit"
    l <- getLine
    let r = parseResult l
    return r
