import Data.List
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ score $ concatMap parseLine $ lines input

score :: [(Int, Int)] -> Int
score = length . filter (> 1) . map length . group . sort

parseLine :: String -> [(Int, Int)]
parseLine s = [(x + x', y + y') | x' <- [0 .. dx - 1], y' <- [0 .. dy - 1]]
  where
    strSplit = splitOn " " s
    xy = splitOn "," $ init $ strSplit !! 2
    x = read (head xy) :: Int
    y = read (last xy) :: Int
    dxdy = splitOn "x" $ strSplit !! 3
    dx = read (head dxdy) :: Int
    dy = read (last dxdy) :: Int
