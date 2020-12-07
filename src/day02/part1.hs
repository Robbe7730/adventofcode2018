main :: IO ()
main = do
    f <- readFile "input.txt"
    print $ uncurry (*) $ foldl getLTThree (0, 0) $ lines f

getLTThree :: (Int, Int) -> String -> (Int, Int)
getLTThree (t', d') "" = (t', d')
getLTThree (t', d') (x:xs)
    | c == 1 = (t' + min 1 (t + 1), d' + d)
    | c == 2 = (t' + t, d' + min 1 (d + 1))
    | otherwise = (t' + t, d' + d)
  where
    c = length $ filter (== x) xs
    (t, d) = getLTThree (0, 0) $ filter (/= x) xs
