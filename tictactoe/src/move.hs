module Move where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Either

import Parser
import MoveDataType
import Winner

getMyMark :: [Move] -> Char
getMyMark [] = 'X'
getMyMark moves = if mark (last moves) == 'X' then 'O' else 'X'

getBoardWithMove :: [Move] -> String -> Char -> [Move]
getBoardWithMove moves id mark =
    let
        move = makeMove moves id mark
        newBoard = moves ++ [move]
    in
        newBoard

makeMove :: [Move] -> String -> Char -> Move
makeMove madeMoves myId myMark =
    let
        possibleMoves = board \\ madeMoves
        possibleGames = getNewGames myMark madeMoves possibleMoves []
        scores = map (\m -> minimax m myMark (swapMark myMark)) possibleGames
        bestScoreIndex = fromJust (elemIndex (maximum scores) scores)
        bestMove = last (possibleGames !! bestScoreIndex)
    in
        setId myId bestMove

minimax :: [Move] -> Char -> Char -> Int
minimax madeMoves myMark currMark =
    let
        possibleMoves = board \\ madeMoves
        possibleGames = getNewGames currMark madeMoves possibleMoves []
        scores = map (\m -> minimax m myMark (swapMark currMark)) possibleGames
    in
        if isGameOver madeMoves
            then gameScore madeMoves myMark
            else if myMark == currMark
                then maximum scores
                else minimum scores

getNewGames :: Char -> [Move] -> [Move] -> [[Move]] -> [[Move]]
getNewGames mark madeMoves [] newGames = newGames
getNewGames mark madeMoves possibleMoves newGames = 
    let 
        currMove = setMark mark (head possibleMoves)
        leftPossibleMoves = tail possibleMoves
        games = newGames ++ [madeMoves ++ [currMove]]
    in
        getNewGames mark madeMoves leftPossibleMoves games

gameScore :: [Move] -> Char -> Int
gameScore moves mark =
    let
        winningMark = getWinningMark moves
    in
        case winningMark of
            Nothing -> 0
            Just value -> if value == mark then 10 else (-10)

swapMark :: Char -> Char
swapMark 'X' = 'O'
swapMark 'O' = 'X'

isGameOver :: [Move] -> Bool
isGameOver moves = if (length moves == 9 || getWinningMark moves /= Nothing) then True else False

board = [Move 0 0 "E" 'E', Move 1 0 "E" 'E', Move 2 0 "E" 'E',
         Move 0 1 "E" 'E', Move 1 1 "E" 'E', Move 2 1 "E" 'E',
         Move 0 2 "E" 'E', Move 1 2 "E" 'E', Move 2 2 "E" 'E']