import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ boardToString $ steps $ parseBoard $ lines input

step :: [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)]
step = map (\(x, y, dx, dy) -> (x + dx, y + dy, dx, dy))

steps :: [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)]
steps cs
    | traceShowId (size cs) >= size nextStep = steps nextStep
    | otherwise = cs
  where
    nextStep = step cs

size :: [(Int, Int, Int, Int)] -> Int
size ps =
    let (maxX, minX, maxY, minY, _) = getMinMaxXY ps
     in (maxX - minX) * (maxY - minY)

getMinMaxXY :: [(Int, Int, Int, Int)] -> (Int, Int, Int, Int, [(Int, Int)])
getMinMaxXY ps =
    let coords = map (\(a, b, _, _) -> (a, b)) ps
        maxX = maximum $ map fst coords
        minX = minimum $ map fst coords
        maxY = maximum $ map snd coords
        minY = minimum $ map snd coords
     in (maxX, minX, maxY, minY, coords)

boardToString :: [(Int, Int, Int, Int)] -> String
boardToString ps =
    let (maxX, minX, maxY, minY, coords) = getMinMaxXY ps
     in unlines
            [ [ if (x, y) `elem` coords
                then 'X'
                else ' '
            | x <- [minX .. maxX]
            ]
            | y <- [minY .. maxY]
            ]

parseBoard :: [String] -> [(Int, Int, Int, Int)]
parseBoard = map parseLine

parseLine :: String -> (Int, Int, Int, Int)
parseLine s =
    let strSplit = words $ filter (`elem` "0123456789- ") s
        (xStr:yStr:dxStr:dyStr:_) = strSplit
        x = read xStr :: Int
        y = read yStr :: Int
        dx = read dxStr :: Int
        dy = read dyStr :: Int
     in (x, y, dx, dy)
