import Data.Char (digitToInt)
import qualified Data.HashMap as M
import Data.List
import Data.List.Index
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "testinput.txt"
    print $ getScore (read $ head $ lines input :: Int) (M.fromList [(0, 3), (1, 7)]) (0, 1) 2

getScore :: Int -> M.Map Int Int -> (Int, Int) -> Int -> Int
getScore i l (a, b) len
    | subListStr l (len - inLen) inLen == show i = len - inLen
    | subListStr l (len - inLen - 1) inLen == show i = len - inLen - 1
    | otherwise = traceShow len $ getScore i nextList nextElves nextLen
  where
    (nextList, nextElves, nextLen) = getNextStep l (a, b) len
    inLen = ceiling $ logBase 10 $ fromIntegral i + 0.1

subListStr :: M.Map Int Int -> Int -> Int -> String
subListStr m p l = concatMap (\x -> show $ M.findWithDefault 0 x m) [p .. p + l - 1]

getNextStep :: M.Map Int Int -> (Int, Int) -> Int -> (M.Map Int Int, (Int, Int), Int)
getNextStep l (a, b) len =
    let aScore = l M.! a
        bScore = l M.! b
        (newList, numAdded) = getNewList l (aScore + bScore) len
        newLen = len + numAdded
        newA = (a + 1 + aScore) `mod` newLen
        newB = (b + 1 + bScore) `mod` newLen
     in (newList, (newA, newB), newLen)

getNewList :: M.Map Int Int -> Int -> Int -> (M.Map Int Int, Int)
getNewList l 0 b = (M.insert b 0 l, 1)
getNewList l s b =
    let numToAdd = ceiling $ logBase 10 $ fromIntegral s + 0.1
        newList =
            foldl'
                (\m x -> M.insert (b + (numToAdd - x - 1)) ((s `div` 10 ^ x) `mod` 10) m)
                l
                [0 .. numToAdd - 1]
     in (newList, numToAdd)
