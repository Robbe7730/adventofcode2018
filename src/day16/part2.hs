import Data.Bits
import qualified Data.HashMap as M
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Debug.Trace

opsInOrder =
    [eqri, bori, addi, bani, seti, eqrr, addr, gtri, borr, gtir, setr, eqir, mulr, muli, gtrr, banr]

type Operator = (Int, Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int, Int)

main :: IO ()
main = do
    input <- readFile "input_part2.txt"
    print $ foldl executeInstruction (0, 0, 0, 0) $ lines input

executeInstruction :: (Int, Int, Int, Int) -> String -> (Int, Int, Int, Int)
executeInstruction rs s = traceShow rs $ op rs (a, b, c)
  where
    (opIdx, a, b, c) = parseOperation s
    op = opsInOrder !! opIdx

parseOperation :: String -> (Int, Int, Int, Int)
parseOperation s =
    let (a:b:c:d:_) = map (\x -> read x :: Int) $ splitOn " " s
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
