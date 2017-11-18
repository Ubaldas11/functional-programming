module Validator where

import MoveDataType

validate :: [Move] -> Either String Bool
validate moves =
    let 
        correctOrder = correctMoveOrder (mark (head moves)) moves
        movesUnique = areMovesUnique moves
    in
        case (movesUnique, correctOrder) of
            (_, False) -> Left "Two X or O placed in a row"
            (False, _) -> Left "There are overlapping moves"
            (_, _) -> Right True

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