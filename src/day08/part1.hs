import Debug.Trace

data Tree =
    Tree [Tree]
         [Int]
    deriving (Show)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ countMetadata $ fst $ parseTree (map read $ words $ head $ lines input) 0

countMetadata :: Tree -> Int
countMetadata (Tree [] md) = sum md
countMetadata (Tree cs md) = sum md + sum (map countMetadata cs)

parseTree :: [Int] -> Int -> (Tree, Int)
parseTree ns x =
    let (n:m:_) = drop x ns
        (children, s) = foldl (parseChildren ns) ([], x + 2) [1 .. n]
     in (Tree children (take m $ drop s ns), s + m)

parseChildren :: [Int] -> ([Tree], Int) -> Int -> ([Tree], Int)
parseChildren t (lt, x) i =
    let (tree, s) = parseTree t x
     in (tree : lt, s)
