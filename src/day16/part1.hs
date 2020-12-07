import Data.Bits
import qualified Data.HashMap as M
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Debug.Trace

ops =
    [ ("addr", addr)
    , ("addi", addi)
    , ("mulr", mulr)
    , ("muli", muli)
    , ("banr", banr)
    , ("bani", bani)
    , ("borr", borr)
    , ("bori", bori)
    , ("setr", setr)
    , ("seti", seti)
    , ("gtir", gtir)
    , ("gtri", gtri)
    , ("gtrr", gtrr)
    , ("eqir", eqir)
    , ("eqri", eqri)
    , ("eqrr", eqrr)
    ]

type Operator = (Int, Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int, Int)

main :: IO ()
main = do
    input <- readFile "input_part1.txt"
    print $ testInstructions $ lines input

testInstructions :: [String] -> Int
testInstructions [] = 0
testInstructions (bStr:oStr:aStr:_:r) =
    let (aBefore, bBefore, cBefore, dBefore) = parseBeforeAfter bStr
        (op, a, b, c) = parseOperation oStr
        (aAfter, bAfter, cAfter, dAfter) = parseBeforeAfter aStr
        resultSet =
            testInstruction
                (aBefore, bBefore, cBefore, dBefore)
                (a, b, c)
                (aAfter, bAfter, cAfter, dAfter)
        subInstructions = testInstructions r
     in subInstructions +
        if S.size resultSet >= 3
            then 1
            else 0

testInstruction :: (Int, Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int, Int) -> S.Set String
testInstruction rs op expRs = S.fromList $ map fst $ filter (\(s, o) -> o rs op == expRs) ops

parseOperation :: String -> (Int, Int, Int, Int)
parseOperation s =
    let (a:b:c:d:_) = map (\x -> read x :: Int) $ splitOn " " s
     in (a, b, c, d)

parseBeforeAfter :: String -> (Int, Int, Int, Int)
parseBeforeAfter s =
    let numbersOnly = drop 9 $ take (length s - 1) s
        (a:b:c:d:_) = map (\x -> read x :: Int) $ splitOn ", " numbersOnly
     in (a, b, c, d)

addr :: Operator
addr rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a + getTupleIdx rs b

addi :: Operator
addi rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a + b

mulr :: Operator
mulr rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a * getTupleIdx rs b

muli :: Operator
muli rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a * b

banr :: Operator
banr rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a .&. getTupleIdx rs b

bani :: Operator
bani rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a .&. b

borr :: Operator
borr rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a .|. getTupleIdx rs b

bori :: Operator
bori rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a .|. b

setr :: Operator
setr rs (a, b, c) = setTupleIndex rs c $ getTupleIdx rs a

seti :: Operator
seti rs (a, b, c) = setTupleIndex rs c a

gtir :: Operator
gtir rs (a, b, c) =
    setTupleIndex rs c $
    if a > getTupleIdx rs b
        then 1
        else 0

gtri :: Operator
gtri rs (a, b, c) =
    setTupleIndex rs c $
    if getTupleIdx rs a > b
        then 1
        else 0

gtrr :: Operator
gtrr rs (a, b, c) =
    setTupleIndex rs c $
    if getTupleIdx rs a > getTupleIdx rs b
        then 1
        else 0

eqir :: Operator
eqir rs (a, b, c) =
    setTupleIndex rs c $
    if a == getTupleIdx rs b
        then 1
        else 0

eqri :: Operator
eqri rs (a, b, c) =
    setTupleIndex rs c $
    if getTupleIdx rs a == b
        then 1
        else 0

eqrr :: Operator
eqrr rs (a, b, c) =
    setTupleIndex rs c $
    if getTupleIdx rs a == getTupleIdx rs b
        then 1
        else 0

getTupleIdx :: (Int, Int, Int, Int) -> Int -> Int
getTupleIdx (a, b, c, d) 0 = a
getTupleIdx (a, b, c, d) 1 = b
getTupleIdx (a, b, c, d) 2 = c
getTupleIdx (a, b, c, d) 3 = d

setTupleIndex :: (Int, Int, Int, Int) -> Int -> Int -> (Int, Int, Int, Int)
setTupleIndex (a, b, c, d) 0 v = (v, b, c, d)
setTupleIndex (a, b, c, d) 1 v = (a, v, c, d)
setTupleIndex (a, b, c, d) 2 v = (a, b, v, d)
setTupleIndex (a, b, c, d) 3 v = (a, b, c, v)
