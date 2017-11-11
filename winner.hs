module Winner where

import Data.List
import Data.Char

import Parser
import MoveDataType

message = "d 1:c d 1:0i2e 1:1i2e e2:id10:GfYDhPxwal4:prevd1:cd1:0i1e1:1i2ee2:id28:hPHnfXjKxVnUlXPuVvnUtRrHzkpx4:prevd1:cd1:0i2e1:1i1ee2:id10:GfYDhPxwal4:prevd1:cd1:0i0e1:1i1ee2:id28:hPHnfXjKxVnUlXPuVvnUtRrHzkpx4:prevd1:cd1:0i2e1:1i0ee2:id10:GfYDhPxwal1:v1:xe1:v1:oe1:v1:xe1:v1:oe1:v1:xe"

winner :: String -> Either String (Maybe String)
winner "" = Left "ERROR: bencode string cannot be empty"
winner "de" = Right Nothing
winner str = 
    let 
        moves = parseMoves [] str
    in 
        case moves of 
                Left msg -> Left msg
                Right moves ->
                    let 
                        movesUnique = areMovesUnique moves
                        correctOrder = correctMoveOrder (mark (head moves)) moves
                    in
                        case (movesUnique, correctOrder) of
                            (_, False) -> Left "ERROR: Two X or O placed in a row"
                            (False, _) -> Left "ERROR: There are overlapping moves"
                            (_, _) -> Right (getWinner moves) 

correctMoveOrder :: Char -> [Move] -> Bool
correctMoveOrder char [] = True
correctMoveOrder char (m:moves) = 
    let
        nextChar = if char == 'X' then 'O' else 'X'
        currMatch = mark m == char
    in
        currMatch && (correctMoveOrder nextChar moves)

areMovesUnique :: [Move] -> Bool
areMovesUnique [] = True
areMovesUnique (m:moves) = m `notElem` moves && areMovesUnique moves

getWinner :: [Move] -> Maybe String
getWinner moves = 
    let 
        (xMoves, oMoves) = distributeMovesByMark 'X' moves
        xHasThree = (findThreeInColumns 0 xMoves) || (findThreeInRows 0 xMoves) || (findThreeDiagonally xMoves)
        oHasThree = (findThreeInColumns 0 oMoves) || (findThreeInRows 0 oMoves) || (findThreeDiagonally oMoves)
    in
        case (xHasThree, oHasThree) of
            (True, _) -> Just (pId (head oMoves))
            (_, True) -> Just (pId (head xMoves))
            (_, _) -> Nothing

distributeMovesByMark :: Char -> [Move] -> ([Move], [Move])
distributeMovesByMark char moves = partition (\move -> mark move == char) moves

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