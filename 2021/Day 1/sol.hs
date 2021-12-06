import System.IO()

main :: IO()
main = do vals <- getVals "input.txt"
          print (part1 vals)
          print (part2 vals)


getVals :: FilePath -> IO [Int]
getVals path = do contents <- readFile path
                  return (map (read::String->Int) (lines contents))

part1 :: [Int] -> Int
part1 xs = sum $ zipWith (\a b -> if a < b then 1 else 0) xs (tail xs)

part2 :: [Int] -> Int
part2 xs = part1 $ zipWith3 (\a b c -> a+b+c) xs (tail xs) (drop 2 xs)
