import System.IO()
import Data.List.Split

main :: IO()
main = do dirs <- getVals "input.txt"
          let x = parseInput dirs
          print (part1 x)
          print (part2 x)

getVals :: FilePath -> IO [String]
getVals fp = do contents <- readFile fp
                return (lines contents)

parseInput :: [String] -> [(String, Int)]
parseInput ds = map (\d -> ((ls d !! 0), read (ls d !! 1))) ds
    where
        ls d = splitOn " " d

convert2d :: (Int, Int) -> (String, Int) -> (Int, Int)
convert2d (x, y) (d, n)
    | d == "forward" = (x + n, y)
    | d == "up" = (x, y - n)
    | d == "down" = (x, y + n)
    | otherwise = (0,0)

convertAim :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
convertAim (x, y, z) (d, n)
    | d == "forward" = ((x + n), (y + (z * n)), z)
    | d == "up" = (x, y, (z - n))
    | d == "down" = (x, y, (z + n))
    | otherwise = (0,0,0)

part1 :: [(String, Int)] -> Int
part1 xs = x * y
    where
        (x, y) = foldl convert2d (0,0) xs

part2 :: [(String, Int)] -> Int
part2 xs = x * y
    where
        (x, y, z) = foldl convertAim (0,0,0) xs
