{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.Matrix
import Data.List.Split (chunksOf, splitOn)

{-
    PowerShell Timing:
        Milliseconds      : 545
        Ticks             : 5456100
-}

main :: IO()
main = do vals <- getVals "input.txt"
          let mtrx = fromLists (map filterWhtSpc vals)
          print (part1 mtrx)
          print (part2 mtrx)

-- Read file to a 2d string array
getVals :: FilePath -> IO [[String]]
getVals path = do contents <- readFile path
                  return (map (splitOn "") (lines contents))

-- Filter away whitespace and convert the rest to ints
filterWhtSpc :: [String] -> [Int]
filterWhtSpc = map (read::String->Int) . filter (/= "")

-- Get indices for points up, down, left and right of given position,
-- then filter away OOB indices
offsets :: Matrix Int -> (Int, Int) -> [(Int, Int)]
offsets m (x,y) = filter (\(a,b) -> not (a < 1 || a > h || b < 1 || b > l)) ps
    where
        ps = map (\(a,b) -> (a+x,b+y)) [(0,-1),(1,0),(0,1),(-1,0)]
        (l,h) = (ncols m, nrows m)

-- An element can be part of basin if lower than 9
basin :: Matrix Int -> (Int, Int) -> Bool
basin m (x,y) = getElem x y m /= 9

-- Check if all points around are larger than element in matrix at p
checkLow :: Matrix Int -> (Int, Int) -> Bool
checkLow m p = all (\(x,y) -> n < getElem x y m) (offsets m p)
    where
        n = uncurry getElem p m

-- Fins all lows in matrix
findLows :: Matrix Int -> [(Int, Int)]
findLows m = filter (checkLow m) ps
    where
        (l, h) = (ncols m, nrows m)
        ps = concatMap (\x' -> map (x',) [1..l]) [1..h]

-- Apply arithmetic according to part 1 to all low points
part1 :: Matrix Int -> Int
part1 m = sum $ map ((+ 1) . (\(x,y) -> getElem x y m)) (findLows m)

-- Recursive floodfill algorithm to find size of a basin
fill :: Matrix Int -> [(Int, Int)] -> [(Int, Int)] -> Int
fill _ [] vstd = length (nub vstd)
fill m (p:ps) vstd = fill m ps' (vstd ++ [p])
    where
        ps' = filter (basin m) (ps ++ (offsets m p \\ vstd))

-- Take the product of the 3 largest basins
part2 :: Matrix Int -> Int
part2 m = product (take 3 (reverse (sort (map (\p -> fill m [p] []) lows))))
    where
        lows = findLows m
