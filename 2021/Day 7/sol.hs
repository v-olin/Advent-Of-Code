import System.IO()
import Data.List.Split

{-
    PowerShell Timing:
        Milliseconds      : 28
        Ticks             : 289279
-}

-- Get an [Int] input, list all possible midpoints, solve
main :: IO()
main = do vals <- getVals "input.txt"
          let as = [(minimum vals)..(maximum vals)]
          print ("Part 1: " ++ show (solve vals as 1))
          print ("Part 2: " ++ show (solve vals as 2))

-- Read input and map it to an [Int]
getVals :: FilePath -> IO [Int]
getVals path = do contents <- readFile path
                  return (map (read::String->Int) (splitOn "," contents))

getFuel :: [Int] -> Int -> Int -> Int
getFuel (x:xs) p m = x' + getFuel xs p m
    where
        h = abs (x-m)
        x' = if p == 1 then h else  ((h+1)*h) `div` 2
getFuel [] _ _ = 0

-- Calculate all permutations of fuel costs and midpoints
-- and return the smallest one
solve :: [Int] -> [Int] -> Int -> Int
solve xs ls p = minimum $ map (getFuel xs p) ls