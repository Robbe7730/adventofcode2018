import Debug.Trace

main :: IO ()
main = do
    serial <- readFile "input.txt"
    print $ getMax $ makeBoard $ read serial

getMax :: [[Int]] -> (Int, Int)
getMax b = snd $ foldl (getMaxF b) (0, (0, 0)) [(x, y) | x <- [0 .. 298], y <- [0 .. 298]]

getMaxF :: [[Int]] -> (Int, (Int, Int)) -> (Int, Int) -> (Int, (Int, Int))
getMaxF b o@(m, _) p@(x, y)
    | score > m = (score, p)
    | otherwise = o
  where
    score = sum [(b !! (y + dy)) !! (x + dx) | dx <- [0 .. 2], dy <- [0 .. 2]]

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
