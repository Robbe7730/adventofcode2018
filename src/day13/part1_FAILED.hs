import Data.List
import Data.List.Index
import Data.List.Unique
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "testinput.txt"
    print $ getCollision 0 $ parseRoads $ lines input

getCollision :: Int -> ([[Bool]], [(Int, Int, Int, Int, Int)]) -> (Int, Int)
getCollision c (r, cs)
    | null collisions = getCollision ((c + 1) `mod` length cs) (r, newCars)
    | otherwise = head collisions
  where
    movedCar = tickCar r (cs !! c)
    newCars = filter (/= (cs !! c)) cs ++ [movedCar]
    collisions = getCollisions $ traceShowId newCars

tickCar :: [[Bool]] -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
tickCar r c@(x, y, dx, dy, i) =
    checkTurn r $ traceShowId $ checkCrossroads r (x + dx, y + dy, dx, dy, i)

getCollisions :: [(Int, Int, Int, Int, Int)] -> [(Int, Int)]
getCollisions cs = map getXY $ repeated cs

getXY :: (Int, Int, Int, Int, Int) -> (Int, Int)
getXY (a, b, c, d, e) = (a, b)

checkTurn :: [[Bool]] -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
checkTurn r c@(x, y, dx, dy, i)
    | (x + dx) < length r - 1 &&
          (y + dy) < length (r !! x) &&
          (x + dx) >= 0 && (y + dy) >= 0 && (r !! (x + dx)) !! (y + dy) = c
    | otherwise = makeTurn r c

makeTurn :: [[Bool]] -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
makeTurn r (x, y, dx, dy, i)
    | (dx, dy) /= (1, 0) && (x + 1 < length r) && ((r !! (x + 1)) !! y) = (x, y, 1, 0, i)
    | (dx, dy) /= (0, 1) && (y + 1 < length (r !! x)) && ((r !! x) !! (y + 1)) = (x, y, 0, 1, i)
    | (dx, dy) /= (-1, 0) && (x - 1 >= 0) && ((r !! (x - 1)) !! y) = (x, y, -1, 0, i)
    | (dx, dy) /= (0, -1) && (y - 1 >= 0) && ((r !! x) !! (y - 1)) = (x, y, 0, -1, i)

checkCrossroads :: [[Bool]] -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
checkCrossroads r c@(x, y, _, _, _)
    | isCrossroad r (x, y) = trace "NS" $ nextStep c
    | otherwise = c

nextStep :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
nextStep (x, y, 1, 0, 0) = (x, y, 0, -1, 1)
nextStep (x, y, 0, 1, 0) = (x, y, 1, 0, 1)
nextStep (x, y, -1, 0, 0) = (x, y, 0, 1, 1)
nextStep (x, y, 0, -1, 0) = (x, y, -1, 0, 1)
nextStep (x, y, 1, 0, 1) = (x, y, 1, 0, 2)
nextStep (x, y, 0, 1, 1) = (x, y, 0, 1, 2)
nextStep (x, y, -1, 0, 1) = (x, y, -1, 0, 2)
nextStep (x, y, 0, -1, 1) = (x, y, 0, -1, 2)
nextStep (x, y, 1, 0, 2) = (x, y, 0, 1, 0)
nextStep (x, y, 0, 1, 2) = (x, y, -1, 0, 0)
nextStep (x, y, -1, 0, 2) = (x, y, 0, -1, 0)
nextStep (x, y, 0, -1, 2) = (x, y, 1, 0, 0)

isCrossroad :: [[Bool]] -> (Int, Int) -> Bool
isCrossroad r (x, y) =
    (y - 1 >= 0) &&
    (x - 1 >= 0) &&
    (y + 1 < length (r !! x)) &&
    (x + 1 < length r) &&
    ((r !! (x - 1)) !! y) && ((r !! x) !! (y - 1)) && ((r !! (x + 1)) !! y) && ((r !! x) !! (y + 1))

parseRoads :: [String] -> ([[Bool]], [(Int, Int, Int, Int, Int)])
parseRoads s =
    let roads =
            [[((s !! y) !! x) /= ' ' | x <- [0 .. length (s !! y) - 1]] | y <- [0 .. length s - 1]]
        cars = foldl getCars [] $ indexed s
     in (roads, sort cars)

getCars :: [(Int, Int, Int, Int, Int)] -> (Int, String) -> [(Int, Int, Int, Int, Int)]
getCars cs (x, s) = foldl (getCar x) cs $ indexed s

getCar :: Int -> [(Int, Int, Int, Int, Int)] -> (Int, Char) -> [(Int, Int, Int, Int, Int)]
getCar x cs (y, '^') = (x, y, -1, 0, 0) : cs
getCar x cs (y, '<') = (x, y, 0, 1, 0) : cs
getCar x cs (y, 'v') = (x, y, 1, 0, 0) : cs
getCar x cs (y, '>') = (x, y, 0, -1, 0) : cs
getCar x cs (y, _) = cs
