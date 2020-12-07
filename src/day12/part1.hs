import Data.List
import Data.Maybe

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ score (-40) $ fst $ foldl' generations (parseInput $ lines input) [1 .. 20]

score :: Int -> [Bool] -> Int
score _ [] = 0
score x (True:bs) = x + score (x + 1) bs
score x (False:bs) = score (x + 1) bs

generations ::
       ([Bool], [Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Bool])
    -> Int
    -> ([Bool], [Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Bool])
generations (st, fs) _ =
    (applyFunctions fs ([False, False, False, False] ++ st ++ [False, False, False, False]), fs)

applyFunctions :: [Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Bool] -> [Bool] -> [Bool]
applyFunctions fs st@(a:b:c:d:e:_)
    | null options = False : applyFunctions fs (tail st)
    | otherwise = fromJust (head options) : applyFunctions fs (tail st)
  where
    options = filter isJust $ map (\f -> f a b c d e) fs
applyFunctions _ _ = []

parseInput :: [String] -> ([Bool], [Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Bool])
parseInput (h:_:f) =
    let state = map (== '#') $ drop 15 h
        fs = map getFunction f
     in (state, fs)

getFunction :: String -> Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Bool
getFunction (a:b:c:d:e:rs) a' b' c' d' e'
    | a' == (a == '#') &&
          b' == (b == '#') && c' == (c == '#') && d' == (d == '#') && e' == (e == '#') =
        Just $ last rs == '#'
    | otherwise = Nothing
