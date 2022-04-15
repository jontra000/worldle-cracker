module Main where

import Lib (solve, parseInput, filterResults, parseResult, Result, UserAction (Quit, Results))
import qualified Data.Maybe

main :: IO ()
main = do
    input <- readFile "wordle.txt"
    let initWord = "arise"
        ws = parseInput input
    next initWord ws

next :: String -> [String] -> IO ()
next guess wordList = do
    putStrLn guess
    result <- nextLine
    next' result guess wordList 

next' :: UserAction -> String -> [String] -> IO ()
next' Quit _ _ = return ()
next' (Results result) guess wordList =
    let wordList' = filterResults wordList guess result
        guess' = solve wordList'
    in  if null wordList'
            then do
                 putStrLn "No more possible matches. Perhaps you made a mistake?"
                 return ()
            else next guess' wordList'

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
