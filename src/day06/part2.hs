import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (fromListWith, toList)
import Data.Maybe
import Debug.Trace
import GHC.Exts

data Point2d =
    Point2d (Double, Double)
    deriving (Eq, Show, Ord)

coord 0 (Point2d p) = fst p
coord 1 (Point2d p) = snd p

dist2 (Point2d (x, y)) (Point2d (x', y')) = abs (x - x') + abs (y - y')

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ length $ filter (< 10000) $ toMap $ map readCoord $ lines input

readCoord :: String -> Point2d
readCoord s =
    let sSplit = splitOn ", " s
        a = read (head sSplit) :: Double
        b = read (last sSplit) :: Double
     in Point2d (a, b)

toMap :: [Point2d] -> [Double]
toMap t = concat [[sum $ map (dist2 (Point2d (a, b))) t | a <- [0 .. 500]] | b <- [0 .. 500]]

comparePoints :: Point2d -> Point2d -> Point2d -> Ordering
comparePoints x a b = compare (dist2 x a) (dist2 x b)

score :: [[Maybe Point2d]] -> [(Point2d, Int)]
score l =
    sortWith snd $
    filter (notInfinite l) $ traceShowId $ Data.Map.toList $ foldl addCount Map.empty $ concat l

notInfinite :: [[Maybe Point2d]] -> (Point2d, Int) -> Bool
notInfinite ls (p, _) = Just p `notElem` head ls ++ last ls ++ map head ls ++ map last ls

addCount :: Map.Map Point2d Int -> Maybe Point2d -> Map.Map Point2d Int
addCount m Nothing = m
addCount m (Just p)
    | Map.member p m = Map.insert p (1 + fromJust (Map.lookup p m)) m
    | otherwise = Map.insert p 1 m
