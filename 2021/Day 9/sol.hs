{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.List.Split (chunksOf, splitOn)
-- import Data.Text (chunksOf)

type Matrix = [[Int]]

main :: IO()
main = do vals <- getVals "test.txt"
          let fltrd = map filterWhtSpc vals
        --   print fltrd
          print (part1 fltrd)

getVals :: FilePath -> IO [[String]]
getVals path = do contents <- readFile path
                  return (map (splitOn "") (lines contents))

sizeM :: Matrix -> (Int, Int)
sizeM m = (length m, length (head m))

filterWhtSpc :: [String] -> [Int]
filterWhtSpc = map (read::String->Int) . filter (/= "")

indices :: (Int, Int) -> [(Int, Int)]
indices (m, n) = concatMap (\x -> map (,x) [0..(m-3)]) [0..(n-3)]

matrixOffset :: Matrix -> (Int, Int) -> Matrix
matrixOffset m (x,y) = take 3 (map (head . chunksOf 3 . drop y) (drop x m))

lowPoint :: Matrix -> Bool
lowPoint m = all (n <) [t,r,b,l]
    where
        n = (m !! 1) !! 1
        t = (m !! 0) !! 1
        r = (m !! 1) !! 2
        b = (m !! 2) !! 1
        l = (m !! 1) !! 0

midp :: Matrix -> Int
midp m = (m !! 1) !! 1

part1 :: Matrix -> [Matrix]
part1 m = filter lowPoint (map (matrixOffset m) (indices $ sizeM m))