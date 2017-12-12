module MoveDataType where

import Test.QuickCheck    

data Move = Move {
    x :: Int,
    y :: Int,
    pId :: String,
    mark :: Char
} deriving Show

instance Arbitrary Move where
    arbitrary = Move <$> choose (0, 2) <*> choose (0, 2) <*> arbitrary <*> elements ['O', 'X']

instance Eq Move where
    (Move x1 y1 id1 mark1) == (Move x2 y2 id2 mark2) =
        (x1 == x2) && (y1 == y2)

setMark :: Char -> Move -> Move
setMark c m = m { mark = c }

setId :: String -> Move -> Move
setId id m = m { pId = id }
