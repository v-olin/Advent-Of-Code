{-# LANGUAGE TupleSections #-}

{-
    PowerShell Timing
        TotalMilliseconds : 19,1038
        Ticks             : 191038
-}

import System.IO()
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

-- Synonyms
type Point = (Int, Int)
type Line = (Point, Point)

-- Parse input and show answers
main :: IO()
main = do input <- getVals "input.txt"
          let lines = map parseLine input
          print ("Part 1: " ++ show (solve (filter isNotDiag lines)))
          print ("Part 2: " ++ show (solve lines))

-- Read input from file
getVals :: FilePath -> IO [String]
getVals fp = do contents <- readFile fp
                return (lines contents)

-- Parse string ["x","y"] to Point (x,y)
parsePoint :: [String] -> Point
parsePoint s = (ints !! 0, ints !! 1)
    where
        ints = map (read::String->Int) s

-- Parse string "x1,y1 -> x2,y2" to Line ((x1,y1),(x2,y2))
parseLine :: String -> Line
parseLine s = (points !! 0, points !! 1)
    where
        nums = map (splitOn ",") (splitOn " -> " s)
        points = map parsePoint nums

-- Check if line is vertical or horizontal
isNotDiag :: Line -> Bool
isNotDiag ((x1,y1),(x2,y2)) = (x1 == x2 || y1 == y2)

-- Get all points from line
pointsFromLine :: Line -> [Point]
pointsFromLine ((x1,y1),(x2,y2))
    | x1 == x2 = map (x1,) [(min y1 y2) .. (max y1 y2)]
    | y1 == y2 = map (,y1) [(min x1 x2) .. (max x1 x2)]
    | x1 < x2 && y1 < y2 = zip [x1..x2] [y1..y2]
    | x1 < x2 && y1 > y2 = zip [x1 .. x2] [y1, (y1 -1) .. y2]
    | x1 > x2 && y1 < y2 = zip [x1, (x1 -1) .. x2] [y1 .. y2]
    | otherwise = zip [x1, (x1 -1) .. x2] [y1, (y1 -1) .. y2]

-- Add line to a Map of points and # of collisions
addToMap :: Line -> Map Point Int -> Map Point Int
addToMap l m = foldl (\a p -> Map.insertWith (+) p 1 a) m ps
    where
        ps = pointsFromLine l

-- Add all lines to an empty map and get amount of collisions
solve :: [Line] -> Int
solve ls = Map.size (Map.filter (> 1) m)
    where
        m = foldl (flip addToMap) Map.empty ls