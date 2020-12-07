data RoadTile = RoadTile
    { north :: Bool
    , east :: Bool
    , south :: Bool
    , west :: Bool
    , car :: Maybe Car
    } deriving (Show)

data Direction
    = North
    | East
    | South
    | West
    deriving (Show)

data Turn
    = LeftTurn
    | RightTurn
    | Forward
    deriving (Show)

instance Enum Turn where
    fromEnum LeftTurn = 0
    fromEnum RightTurn = 1
    fromEnum Forward = 2
    toEnum 0 = LeftTurn
    toEnum 1 = RightTurn
    toEnum 2 = Forward
    toEnum x = toEnum (x `mod` 3)

type Road = [[RoadTile]]

data Car = Car
    { position :: (Int, Int)
    , heading :: Direction
    } deriving (Show)

main :: IO ()
main = do
    input <- readFile "testinput_modified.txt"
    print $ parseRoads $ lines input

parseRoads :: [String] -> Road
parseRoads = map (map getTile)

getTile :: Char -> RoadTile
getTile '-' = RoadTile False True False True Nothing
getTile '|' = RoadTile True False True False Nothing
getTile 'A' = RoadTile False True True False Nothing
getTile 'B' = RoadTile True False False True Nothing
getTile 'C' = RoadTile True True False False Nothing
getTile 'D' = RoadTile False False True True Nothing
getTile '+' = RoadTile True True True True Nothing
getTile _ = RoadTile False False False False Nothing
