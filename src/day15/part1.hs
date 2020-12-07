import Data.List.Index
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import GHC.Exts

width = 9

height = 9

data Character
    = Elf { hp :: Int }
    | Goblin { hp :: Int }
    | Wall
    deriving (Show, Eq)

type Field = M.Map (Int, Int) Character

doesAttack :: Character -> Character -> Bool
doesAttack (Elf _) (Goblin _) = True
doesAttack (Goblin _) (Elf _) = True
doesAttack _ _ = False

main :: IO ()
main = do
    input <- readFile "testinput.txt"
    putStrLn $ printMap $ doMove $ readMap $ lines input

doMove :: Field -> Field
doMove m = M.map (doCharacterMove m) m

doCharacterMove :: Field -> Character -> Character
doCharacterMove _ w@(Wall _) = w
doCharacterMove m c = tryAttack m $ tryMove m c

tryMove :: Field -> Character -> Character
tryMove m c
    | isJust nearestCharacter = moveTowards m c $ fromJust nearestCharacter
    | otherwise = c
  where
    nearestCharacter = getNearestCharacter m c

moveTowards :: Field -> Character -> Character -> Character
moveTowards m s t =
    let (x, y) = position s
        possibleSteps = [(x + dx, y + dy) | (dx, dy) <- [(0, 1), (-1, 0), (1, 0), (0, -1)]]
     in minimumWith

getNearestCharacter :: Field -> Character -> Maybe Character
getNearestCharacter m c = bfsCharacter m S.empty [position c]

bfsCharacter :: Field -> S.Set (Int, Int) -> [(Int, Int)] -> Maybe Character
bfsCharacter _ _ [] = Nothing
bfsCharacter m v (c@(x, y):s)
    | null found = bfsCharacter m (S.insert c v) $ s ++ nextSteps
    | otherwise = Just $ head found
  where
    found = filter (\x -> position x == c) m
    nextSteps = filter (`S.notMember` v) [(x, y + 1), (x - 1, y), (x + 1, y), (x, y + 1)]

tryAttack :: Field -> Character -> Character
tryAttack = const id

printMap :: Field -> String
printMap cs = unlines $ chunksOf width $ foldl addCharacter (replicate (width * height) '.') cs

addCharacter :: String -> Character -> String
addCharacter s c = take n s ++ [getCharacterStr c] ++ drop (n + 1) s
  where
    (x, y) = position c
    n = y * height + x

getCharacterStr :: Character -> Char
getCharacterStr (Elf _) = 'E'
getCharacterStr (Goblin _) = 'G'
getCharacterStr Wall = '#'

readMap :: [String] -> M.Map (Int, Int) Character
readMap m = foldl (\a (y, e) -> foldl (getCharacter y) a $ indexed e) M.empty $ indexed m

getCharacter :: Int -> Field -> (Int, Char) -> Field
getCharacter y cs (x, 'E') = M.insert (x, y) (Elf 200) cs
getCharacter y cs (x, 'G') = M.insert (x, y) (Goblin 200) cs
getCharacter y cs (x, '#') = M.insert (x, y) Wall cs
getCharacter y cs (x, '.') = cs
