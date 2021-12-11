{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.Maybe
import Data.Stack
import Data.List.Split (chunksOf, splitOn)

{-
    PowerShell Timing:
        Milliseconds      : 45
        Ticks             : 452883
-}

main :: IO()
main = do vals <- getVals "input.txt"
          let series = map (filter (/= "")) vals
          print (sum $ part1 series)
          print (part2 series)

getVals :: FilePath -> IO [[String]]
getVals path = do contents <- readFile path
                  return (map (splitOn "") (lines contents))

inverse :: String -> String
inverse s = case s of "(" -> ")"
                      "[" -> "]"
                      "{" -> "}"
                      "<" -> ">"
                      _ -> ""

points :: String -> Int
points s = case s of ")" -> 3
                     "]" -> 57
                     "}" -> 1197
                     ">" -> 25137
                     "(" -> 1
                     "[" -> 2
                     "{" -> 3
                     "<" -> 4
                     _ -> 0

isOpen :: String -> Bool
isOpen s = s `elem` ["(","[","{","<"]

runLine :: Stack String -> [String] -> (Stack String, String)
runLine stack [] = (stack, "")
runLine stack (s:ss)
    | isOpen s = runLine (stackPush stack s) ss
    | stackIsEmpty stack = (stack, s)
    | inverse top == s = runLine pop ss
    | otherwise = (stack, s)
    where
        top = fromMaybe "!" (stackPeek stack)
        pop = fst (fromJust (stackPop stack))

popAll :: Stack String -> [String]
popAll stack
    | isNothing popd = []
    | otherwise = snd (fromJust popd) : popAll (fst (fromJust popd))
    where
        popd = stackPop stack

foldScore :: [Int] -> Int
foldScore = foldl (\x y -> x*5 + y) 0

part1 :: [[String]] -> [Int]
part1 = map (points . snd . runLine stackNew)

part2 :: [[String]] -> Int
part2 ss = sort scores !! (length scores `div` 2)
    where
        stacks = filter (\x -> snd x == "") (map (runLine stackNew) ss)
        scores = map (foldl (\x y -> x*5 + y) 0 . map points . popAll . fst) stacks