import System.IO()
import Data.List.Split

{-
    PowerShell Timing:
        Milliseconds      : 298
        Ticks             : 2981559
-}

-- Get an [Int] input, list all possible midpoints, solve
main :: IO()
main = do vals <- getVals "input.txt"
          let as = [(maximum vals)..(minimum vals)]
          print ("Part 1: " ++ show (solve vals as 1))
          print ("Part 2: " ++ show (solve vals as 2))

-- Read input and map it to an [Int]
getVals :: FilePath -> IO [Int]
getVals path = do contents <- readFile path
                  return (map (read::String->Int) (splitOn "," contents))

-- Calculate the fuel needed for each submarine
-- to move from x to m (midpoint)
getFuel :: [Int] -> Int -> Int -> Int
getFuel xs p m = foldl (+) 0 (xs' p m)
    where
        xs' p m = map (\x -> if p == 1 then abs (x-m) else sum [1..(abs (x-m))]) xs

-- Calculate all permutations of fuel costs and midpoints
-- and return the smallest one
solve :: [Int] -> [Int] -> Int -> Int
solve xs ls p = minimum $ map (getFuel xs p) ls