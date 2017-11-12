import Parser
import MoveDataType
import Control.Monad
import Debug.Trace
import Data.Either

--visad gaus validzios lentos ejimus (ne pilna, nesidubliuojancia, t.t.) ir grazins ejima
move :: [Move] -> Either String (Int, Int, Char)
move moves = do
  mark <- myMark moves
  isFirst <- evenMoveNumber moves
  return $ case isFirst of
    True -> attack moves mark
    False -> defend moves mark

myMark :: [Move] -> Either String Char
myMark [] = Right 'X'
myMark moves = if mark (last moves) == 'X' then Right 'O' else Right 'X'

evenMoveNumber :: [Move] -> Either String Bool
evenMoveNumber moves = Right ((length moves) `mod` 2 == 0)

attack :: [Move] -> Char -> (Int, Int, Char)
attack [] mark = (1, 1, mark)
attack moves mark = 
    let
        lastMove = last moves
        x' = mirrorCoord (x lastMove)
        y' = mirrorCoord (y lastMove)
    in
        (x', y', mark)

mirrorCoord :: Int -> Int
mirrorCoord 0 = 2
mirrorCoord 1 = 1
mirrorCoord 2 = 0

defend :: [Move] -> Char -> (Int, Int, Char)
defend _ _ = (999, 999, 'D')

t :: String -> Either String (Int, Int, Char)
t str = 
    let
        moves' = parseMoves [] str
    in
        case moves' of
            Right moves -> move moves
            Left msg -> Left msg