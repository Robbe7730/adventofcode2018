import Data.Char
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import GHC.Exts

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $
        traverseTree (replicate 5 (0, 'a')) $ completeTree $ foldl parseTree Map.empty $ lines input

traverseTree :: [(Int, Char)] -> Map.Map Char String -> Int
traverseTree p@((ta, ca):ps) m
    | Map.null m = traceShow p $ maximum $ map fst ps
    | otherwise = traverseTree (sort $ (nextTime, nextTask) : ps) nextMap
  where
    nextTasks = traceShow p $ Map.keys $ Map.filter null $ Map.map (filter (/= ca)) m
    nextTask =
        if null nextTasks
            then 'a'
            else minimum nextTasks
    nextTime =
        if null nextTasks
            then ta + 1
            else ta + ord nextTask - 4
    nextMap = Map.map (filter (/= ca)) $ Map.filterWithKey (\k _ -> k /= nextTask) m

completeTree :: Map.Map Char String -> Map.Map Char String
completeTree m = foldl (`tryInsert` "") m $ Map.keys m ++ concat (Map.elems m)

tryInsert :: (Ord k) => Map.Map k v -> v -> k -> Map.Map k v
tryInsert m val key
    | Map.member key m = m
    | otherwise = Map.insert key val m

parseTree :: Map.Map Char String -> String -> Map.Map Char String
parseTree map s =
    let from = (s !! 5)
        to = (s !! 36)
     in Map.insert to (from : Map.findWithDefault [] to map) map
