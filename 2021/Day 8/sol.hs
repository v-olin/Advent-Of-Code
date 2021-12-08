import System.IO()
import Data.List
import Data.List.Split

main :: IO()
main = do ls <- getVals "input.txt"
          let parsed = parseCombos ls
          print ("Part 1: " ++ show (part1 parsed))

type Combo = ([String], [String])

p1digits :: [Int]
p1digits = [2, 4, 3, 7]

p2digits :: [(Int,String)]
p2digits = [(0,"a")]

-- Filter away whitespaces or newlines
noWhtSpc :: String -> Bool
noWhtSpc s = if s == "" then False else True

getVals :: FilePath -> IO [String]
getVals path = do contents <- readFile path
                  return (lines contents)

parseCombo :: String -> Combo
parseCombo s = (s' !! 0, s' !! 1)
    where
        s' = map (filter noWhtSpc) (map (splitOn " ") (splitOn "|" s))

parseCombos :: [String] -> [Combo]
parseCombos ss = map parseCombo ss

checkLen :: [String] -> Int
checkLen ss = length $ intersect ss' p1digits
    where
        ss' = map (\s -> length s) ss

part1 :: [Combo] -> Int
part1 cs = foldl (+) 0 $ map (\(a,b) -> checkLen b) cs

