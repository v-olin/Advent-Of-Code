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

-- Read file to a 2d array of chars as strings
getVals :: FilePath -> IO [[String]]
getVals path = do contents <- readFile path
                  return (map (splitOn "") (lines contents))

-- The corresponding closing bracket of an opening bracket
inverse :: String -> String
inverse s = case s of "(" -> ")"
                      "[" -> "]"
                      "{" -> "}"
                      "<" -> ">"
                      _ -> ""

-- Points for each bracket depending on part1 or part2
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

-- If bracket is an opening bracket
isOpen :: String -> Bool
isOpen s = s `elem` ["(","[","{","<"]

-- Goes through all brackets until first mis-match happens, then
-- return the current state of the stack and the mis-matched char
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

-- Convert stack to list
popAll :: Stack String -> [String]
popAll stack
    | isNothing popd = []
    | otherwise = snd (fromJust popd) : popAll (fst (fromJust popd))
    where
        popd = stackPop stack

-- Get all mis-matched characters and convert to points
part1 :: [[String]] -> [Int]
part1 = map (points . snd . runLine stackNew)

-- Get all incomplete stacks, map the stacks to a list of points,
-- fold them and find the middle score
part2 :: [[String]] -> Int
part2 ss = sort scores !! (length scores `div` 2)
    where
        stacks = filter (\x -> snd x == "") (map (runLine stackNew) ss)
        scores = map (foldl (\x y -> x*5 + y) 0 . map points . popAll . fst) stacks