main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ result $ head $ filter (\x -> diff x == 1) $ carthProduct $ lines input

carthProduct :: [a] -> [(a, a)]
carthProduct xs = [(a, b) | a <- xs, b <- xs]

diff :: (String, String) -> Int
diff ("", "") = 0
diff (x:xs, y:ys)
    | x == y = r
    | otherwise = 1 + r
  where
    r = diff (xs, ys)

result :: (String, String) -> String
result (a, b) = filter (`elem` b) a
