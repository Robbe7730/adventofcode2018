import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import GHC.Exts

data Action
    = Wake
    | Sleep
    | NewGuard Int
    deriving (Show)

main :: IO ()
main = do
    x <- readFile "input.txt"
    print $
        (\(a, _, b) -> a * b) $
        Map.foldrWithKey todo (0, -1, 0) $
        thr $ foldl countSleep (0, 0, Map.empty) $ sortWith fst $ map parseLine $ lines x

todo :: Int -> [Int] -> (Int, Int, Int) -> (Int, Int, Int)
todo k vs (mg, ms, mm)
    | maxSleep > ms = (k, maxSleep, maxMin)
    | otherwise = (mg, ms, mm)
  where
    maxSleep = maximum vs
    maxMin = fromJust $ elemIndex maxSleep vs

thr :: (x, y, z) -> z
thr (_, _, a) = a

parseLine :: String -> (String, Action)
parseLine s =
    let dateStr = (take 16 $ drop 1 s)
        action = parseAction $ drop 19 s
     in (dateStr, action)

parseAction :: String -> Action
parseAction "wakes up" = Wake
parseAction "falls asleep" = Sleep
parseAction s = NewGuard $ read $ drop 7 $ take (length s - 13) s

countSleep :: (Int, Int, Map.Map Int [Int]) -> (String, Action) -> (Int, Int, Map.Map Int [Int])
countSleep (_, s, m) (_, NewGuard g) = (g, 0, m)
countSleep (g, s, m) (t, Sleep) = (g, read $ drop 14 t, m)
countSleep (g, s, m) (t, Wake) = (g, 0, Map.insert g times m)
  where
    oldTimes = Map.findWithDefault (replicate 60 0) g m
    times =
        [ if i >= s && i < read (drop 14 t)
            then 1 + oldTimes !! i
            else oldTimes !! i
        | i <- [0 .. 59]
        ]
