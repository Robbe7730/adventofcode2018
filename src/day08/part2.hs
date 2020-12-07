import Debug.Trace

data Tree =
    Tree [Tree]
         [Int]
    deriving (Show)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ score $ fst $ parseTree (map read $ words $ head $ lines input) 0

score :: Tree -> Int
score t@(Tree [] md) = sum md
score t@(Tree cs md) = sum [score (cs !! (i - 1)) | i <- filter (< (length cs + 1)) md]

parseTree :: [Int] -> Int -> (Tree, Int)
parseTree ns x =
    let (n:m:_) = drop x ns
        (children, s) = foldl (parseChildren ns) ([], x + 2) [1 .. n]
     in (Tree (reverse children) (take m $ drop s ns), s + m)

parseChildren :: [Int] -> ([Tree], Int) -> Int -> ([Tree], Int)
parseChildren t (lt, x) i =
    let (tree, s) = parseTree t x
     in (tree : lt, s)
