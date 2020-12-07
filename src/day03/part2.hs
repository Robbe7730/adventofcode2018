import Data.List
import Data.List.Split
import Data.Ord
import Debug.Trace
import GHC.Exts (sortWith)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ getAnswer $ getOverlapping $ toGrid $ concatMap parseLine $ lines input

getAnswer :: [Int] -> [Int]
getAnswer xs = filter (`notElem` xs) [1 .. 1331]

getOverlapping :: [[(Int, Int, Int)]] -> [Int]
getOverlapping [] = []
getOverlapping (p:ps)
    | length p == 1 = r
    | otherwise = o ++ r
  where
    r = getOverlapping ps
    o = map getThird p

getThird :: (a, a, a) -> a
getThird (_, _, x) = x

toGrid :: [(Int, Int, Int)] -> [[(Int, Int, Int)]]
toGrid = groupBy groupTile . sortWith (\(a, b, _) -> (a, b))

groupTile :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
groupTile (a, b, _) (a', b', _) = (a, b) == (a', b')

parseLine :: String -> [(Int, Int, Int)]
parseLine s = [(x + x', y + y', label) | x' <- [0 .. dx - 1], y' <- [0 .. dy - 1]]
  where
    strSplit = splitOn " " s
    xy = splitOn "," $ init $ strSplit !! 2
    x = read (head xy) :: Int
    y = read (last xy) :: Int
    dxdy = splitOn "x" $ strSplit !! 3
    dx = read (head dxdy) :: Int
    dy = read (last dxdy) :: Int
    label = read $ tail $ head strSplit :: Int
