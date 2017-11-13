import Parser
import MoveDataType
import Winner
import Control.Monad
import Data.List

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
attack moves mark = (mirrorCoord (x (last moves)), mirrorCoord (y (last moves)), mark)

mirrorCoord :: Int -> Int
mirrorCoord 0 = 2
mirrorCoord 1 = 1
mirrorCoord 2 = 0

defend :: [Move] -> Char -> (Int, Int, Char)
defend allMoves myMark = 
    let
        (myMoves, oppMoves) = distributeMovesByMark myMark allMoves
        emptyMoves = board \\ allMoves
        moveInSameLane = findMoveInSameLane myMoves oppMoves emptyMoves
    in
        case moveInSameLane of
            Nothing -> (5, 5, myMark)
            Just value -> (fst value, snd value, myMark)


findMoveInSameLane :: [Move] -> [Move] -> [Move] -> Maybe (Int, Int)
-- neradom savo ejimo, kuris savam lane turetu priesininko ejima
findMoveInSameLane [] oppMoves emptyMoves = Nothing
findMoveInSameLane myMoves oppMoves emptyMoves = 
    let 
        myMove = head myMoves
        oppMoveInAxisX = find (\mo -> x mo == x myMove) oppMoves
        oppMoveInAxisY = find (\mo -> y mo == y myMove) oppMoves
        nextTry = findMoveInSameLane (tail myMoves) oppMoves emptyMoves
    in
        case (oppMoveInAxisX, oppMoveInAxisY) of
            (Nothing, Nothing) -> nextTry
            (oppMoveX, oppMoveY) -> 
                let
                    foundEmptySpot = findEmptySpot myMove oppMoveX oppMoveY emptyMoves
                in
                    case foundEmptySpot of
                        Nothing -> nextTry
                        Just value -> foundEmptySpot

findEmptySpot :: Move -> Maybe Move -> Maybe Move -> [Move] -> Maybe (Int, Int)
findEmptySpot myMove Nothing Nothing emptyMoves = Nothing
findEmptySpot myMove (Just oppMoveX) oppMoveY emptyMoves =
    let
        emptyMove = find(\m -> x m == x oppMoveX) emptyMoves
    in 
        case emptyMove of
            Nothing -> findEmptySpot myMove Nothing oppMoveY emptyMoves
            Just value -> Just (x value, y value)
findEmptySpot myMove Nothing (Just oppMoveY) emptyMoves = 
    let
        emptyMove = find(\m -> y m == y oppMoveY) emptyMoves
    in
        case emptyMove of
            Nothing -> Nothing
            Just value -> Just (x value, y value)

board = [Move 0 0 "E" 'E', Move 1 0 "E" 'E', Move 2 0 "E" 'E',
         Move 0 1 "E" 'E', Move 1 1 "E" 'E', Move 2 1 "E" 'E',
         Move 0 2 "E" 'E', Move 1 2 "E" 'E', Move 2 2 "E" 'E']

t :: String -> Either String (Int, Int, Char)
t str = 
    let
        moves' = parseMoves [] str
    in
        case moves' of
            Right moves -> move moves
            Left msg -> Left msg