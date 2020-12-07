import Debug.Trace

main :: IO ()
main = do
    s <- readFile "input.txt"
    print $ sum $ map ((read :: String -> Int) . filter (/= '+')) $ lines s
