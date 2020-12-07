import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ score $ playGame input

score :: ([Int], Int, Int, [Int]) -> Int
score (_, _, _, ss) = maximum ss

playGame :: String -> ([Int], Int, Int, [Int])
playGame input =
    let inputSplit = words $ head $ lines input
        numPlayers = read $ head inputSplit
        lastBall = read $ last $ init inputSplit
     in foldl (addBall numPlayers) ([0], 0, 0, replicate numPlayers 0) [1 .. lastBall]

addBall :: Int -> ([Int], Int, Int, [Int]) -> Int -> ([Int], Int, Int, [Int])
addBall np (bs, l, p, ss) c
    | c `mod` 23 == 0 =
        ( filter (/= marbleToRemove) bs
        , ballPosition (-7) 0
        , (p + 1) `mod` np
        , addScore ss p (c + marbleToRemove))
    | otherwise = (insertBall bs (ballPosition 2 0) c, ballPosition 2 0, (p + 1) `mod` np, ss)
  where
    ballPosition d d' = (l + d) `mod` (length bs + d')
    marbleToRemove = bs !! ballPosition (-6) 0

insertBall :: [Int] -> Int -> Int -> [Int]
insertBall bs n s = take (n + 1) bs ++ [s] ++ drop (n + 1) bs

addScore :: [Int] -> Int -> Int -> [Int]
addScore ss p s = take p ss ++ [(ss !! p) + s] ++ drop (p + 1) ss
