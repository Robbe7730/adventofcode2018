import Data.List
import qualified Data.Vector as V
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "testinput.txt"
    print $ score $ playGame input

score :: (MyList Int, Int, Int, V.Vector Int, Int) -> Int
score (_, _, _, _, m) = m

playGame :: String -> (MyList Int, Int, Int, V.Vector Int, Int)
playGame input =
    let inputSplit = words $ head $ lines input
        numPlayers = read $ head inputSplit
        lastBall = read (last $ init inputSplit)
     in foldl'
            (addBall numPlayers)
            (Cons 1 0 MyNil, 0, 0, V.replicate numPlayers 0, 0)
            [1 .. lastBall]

addBall ::
       Int
    -> (MyList Int, Int, Int, V.Vector Int, Int)
    -> Int
    -> (MyList Int, Int, Int, V.Vector Int, Int)
addBall np (bs, l, p, ss, m) c
    | c `mod` 23 == 0 =
        (newMarbles, ballPosition (-7), (p + 1) `mod` np, scores, traceShowId $ max m newScore)
    | otherwise = (insertBall bs (ballPosition 2) c, ballPosition 2, (p + 1) `mod` np, ss, m)
  where
    ballPosition d = (l + d) `mod` myLength bs
    (newMarbles, removed) = myRemove bs $ ballPosition (-6)
    (scores, newScore) = addScore ss p (c + removed)

insertBall :: MyList Int -> Int -> Int -> MyList Int
insertBall bs n s = myInsert (n + 1) s bs

addScore :: V.Vector Int -> Int -> Int -> (V.Vector Int, Int)
addScore ss p s = (V.update ss $ V.singleton (p, newScore), newScore)
  where
    newScore = (ss V.! p) + s

data MyList a
    = Cons Int
           a
           (MyList a)
    | MyNil
    deriving (Show, Eq)

myRemove :: MyList a -> Int -> (MyList a, a)
myRemove (Cons _ v x) 0 = (x, v)
myRemove (Cons l v x) i =
    let (a, b) = myRemove x (i - 1)
     in (Cons l v a, b)

myHead :: MyList a -> a
myHead l =
    case l of
        Cons _ a _ -> a

myTail :: MyList a -> MyList a
myTail MyNil = MyNil
myTail l =
    case l of
        Cons _ _ a -> a

myIndex :: Int -> MyList a -> a
myIndex 0 xs = myHead xs
myIndex x xs = myHead (myIndexTail x xs)
  where
    myIndexTail 0 xs = xs
    myIndexTail i xs = myIndexTail (i - 1) (myTail xs)

myLength :: MyList a -> Int
myLength MyNil = 0
myLength (Cons l _ _) = l

myLast :: MyList a -> a
myLast (Cons _ a MyNil) = a
myLast l = myLast (myTail l)

myInsert :: Int -> a -> MyList a -> MyList a
myInsert x v MyNil = Cons 1 v MyNil
myInsert 0 v c@(Cons l _ _) = Cons (l + 1) v c
myInsert x v (Cons l a c) = Cons (l + 1) a $ myInsert (x - 1) v c
