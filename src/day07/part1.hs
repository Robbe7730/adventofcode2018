import Data.List
import qualified Data.Map as Map
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ traverseTree $ completeTree $ foldl parseTree Map.empty $ lines input

traverseTree :: Map.Map Char String -> String
traverseTree m
    | Map.null m = ""
    | otherwise =
        let top = minimum $ Map.keys $ Map.filter null m
            other = Map.filterWithKey (\k _ -> k /= top) $ Map.map (filter (/= top)) m
         in top : traverseTree other

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
