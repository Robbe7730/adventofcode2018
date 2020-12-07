import Data.Char

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ minimum $ map (length . reaction) $ filtered $ reaction $ head $ lines input

filtered :: String -> [String]
filtered s = [filter ((/= x) . toLower) s | x <- ['a' .. 'z']]

reaction :: String -> String
reaction = foldl checkReact ""

checkReact :: String -> Char -> String
checkReact [] x = [x]
checkReact s x
    | x == oppositeCase (last s) = init s
    | otherwise = s ++ [x]

oppositeCase :: Char -> Char
oppositeCase c
    | isUpper c = toLower c
    | isLower c = toUpper c
