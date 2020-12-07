import Data.List
import Debug.Trace

main :: IO ()
main = do
    serial <- readFile "input.txt"
    print $ getMax $ makeBoard $ read serial

getMax :: [[Int]] -> (Int, Int, Int)
getMax b = score $ foldl' (getMaxF b) (0, 0, (0, 0)) [(x, y) | x <- [0 .. 300], y <- [0 .. 300]]

score :: (Int, Int, (Int, Int)) -> (Int, Int, Int)
score (_, s, (x, y)) = (x, y, s)

getMaxF :: [[Int]] -> (Int, Int, (Int, Int)) -> (Int, Int) -> (Int, Int, (Int, Int))
getMaxF b o@(m, _, _) (x, y)
    | score > m = (score, s, (x, y))
    | otherwise = o
  where
    subBoard = map (drop x) $ drop y b
    (score, s, _) = getScore subBoard (max 1 $ min (length subBoard) (length (head subBoard)) - 1)

getScore :: [[Int]] -> Int -> (Int, Int, Int)
getScore b 1 = (head $ head b, 1, head $ head b)
getScore b s =
    let (maxScore, maxSize, subScore) = getScore b (s - 1)
        colScore = sum $ map (!! (s - 1)) $ take s b
        rowScore = sum $ take s $ b !! (s - 1)
        dScore
          -- traceShow (s, maxSize, subScore, rowScore, colScore) $
         = rowScore + colScore - (b !! (s - 1)) !! (s - 1)
        currScore = subScore + dScore
     in max (currScore, s, currScore) (maxScore, maxSize, currScore)

makeBoard :: Int -> [[Int]]
makeBoard s = [[calculatePixel s x y | x <- [0 .. 300]] | y <- [0 .. 300]]

calculatePixel :: Int -> Int -> Int -> Int
calculatePixel s x y =
    let rackID = x + 10
        power1 = rackID * y
        power2 = power1 + s
        power3 = power2 * rackID
        power4 = (power3 `div` 100) `mod` 10
     in power4 - 5
