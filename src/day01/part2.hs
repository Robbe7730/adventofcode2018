import Data.Set as Set
import Debug.Trace

main :: IO ()
main = do
    s <- readFile "input.txt"
    print $
        dup $
        sumlist' $
        Prelude.take 1000000 $
        cycle $ Prelude.map ((read :: String -> Int) . Prelude.filter (/= '+')) $ lines s

sumlist' xx = aux xx 0
  where
    aux [] a = []
    aux (x:xs) a = (a + x) : aux xs (a + x)

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where
    dup' [] _ = Nothing
    dup' (x:xs) s =
        if Set.member x s
            then Just x
            else dup' xs (Set.insert x s)
