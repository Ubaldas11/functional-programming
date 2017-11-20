module Encoder where

import MoveDataType

encMovesToStr :: [Move] -> String
encMovesToStr moves = convertMoves moves []

convertMoves :: [Move] -> String -> String
convertMoves [] str = str
convertMoves moves str = 
        let 
            move = head moves
            coordsStr = "d1:cd1:0i" ++ show (x move) ++ "e1:1i" ++ show (y move) ++ "ee"
            idStr = "2:id" ++ show (length (pId move)) ++ ":" ++ pId move
            prevStr = if (str == []) then "" else "4:prev"
            markStr = "1:v1:" ++ [(mark move)] ++ "e"
        in
            convertMoves (tail moves) (coordsStr ++ idStr ++ prevStr ++ str ++ markStr) 