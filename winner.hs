module Winner where

import Data.List
import Data.Char
import Debug.Trace
import Parser
import MoveDataType

message = "d1:cd1:0i1e1:1i0ee2:id14:TwqkxOFfdfdfdmwqdSCK4:prevd1:cd1:0i2e1:1i0ee2:id2:IY4:prevd1:cd1:0i2e1:1i1ee2:id14:TwqkxOFmwqdSCK4:prevd1:cd1:0i1e1:1i2ee2:id2:IY4:prevd1:cd1:0i2e1:1i2ee2:id14:TwqkxOFmwqdSCK1:v1:oe1:v1:oe1:v1:xe1:v1:oe1:v1:xe"

winner :: String -> Either String (Maybe String)
winner "de" = Right Nothing
winner str = 
    let 
        moves = parseMoves [] str
    in 
        case moves of 
                Left msg -> Left msg
                Right moves ->
                                if (areMovesUnique moves) 
                                then Right (getWinner moves)
                                else Left "ERROR: There are overlapping moves"   

areMovesUnique :: [Move] -> Bool
areMovesUnique [] = True
areMovesUnique (m:moves) = m `notElem` moves && areMovesUnique moves

getWinner :: [Move] -> Maybe String
getWinner moves = 
    let 
        (xMoves, oMoves) = distributeMoves 'X' moves
        xHasThree = (findThreeInColumns 0 xMoves) || (findThreeInRows 0 xMoves) || (findThreeDiagonally xMoves)
        oHasThree = (findThreeInColumns 0 oMoves) || (findThreeInRows 0 oMoves) || (findThreeDiagonally oMoves)
    in
        case (xHasThree, oHasThree) of
            (True, _) -> Just (pId (head oMoves))
            (_, True) -> Just (pId (head xMoves))
            (_, _) -> Nothing

distributeMoves :: Char -> [Move] -> ([Move], [Move])
distributeMoves char moves = partition (\move -> mark move == char) moves

findThreeInColumns :: Int -> [Move] -> Bool
findThreeInColumns 3  _ = False
findThreeInColumns counter moves = (length (filter (\move -> x move == counter) moves) == 3) || findThreeInColumns (counter+1) moves

findThreeInRows :: Int -> [Move] -> Bool
findThreeInRows 3  _ = False
findThreeInRows counter moves = (length (filter (\move -> y move == counter) moves) == 3) || findThreeInRows (counter+1) moves

findThreeDiagonally :: [Move] -> Bool
findThreeDiagonally moves = 
    let
        center = find (\move -> x move == 1 && y move == 1) moves
        topLeft = find (\move -> x move == 0 && y move == 0) moves
        topRight = find (\move -> x move == 2 && y move == 0) moves
        botLeft = find (\move -> x move == 0 && y move == 2) moves
        botRight = find (\move -> x move == 2 && y move == 2) moves
        fm Nothing = False
        fm _ = True
    in
        (fm center && fm botLeft && fm topRight) || (fm center && fm topLeft && fm botRight)