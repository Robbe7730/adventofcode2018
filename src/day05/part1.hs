import Data.Char

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ length $ reaction $ head $ lines input

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
