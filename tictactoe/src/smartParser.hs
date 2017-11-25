{-# LANGUAGE FlexibleContexts #-}
module SmartParser where

import Text.Parsec
import Text.Parsec.Token
import Text.ParserCombinators.Parsec hiding (try)
import MoveDataType
import Debug.Trace
import Data.Char
import Control.Monad

message1 = "[\"c\", [0, 0], \"v\", \"o\", \"id\", \"AyleaFLipOnZmGryUvoQgAShRvs\"]  [\"c\", [2, 2], \"v\", \"o\", \"id\", \"AyleaFLipOnZmGryUvoQgAShRvs\"]]"
message = "[\"c\", [1, 0], \"v\", \"x\", \"id\", \"QPkTqUnFnWTMcp\", \"prev\", [\"c\", [1, 1], \"v\", \"o\", \"id\", \"vEyUH\", \"prev\", [\"c\", [0, 2], \"v\", \"x\", \"id\", \"QPkTqUnFnWTMcp\"]]]"

parseMove :: Parser Move
parseMove = do
  string "[\"c\", ["
  fstCoordChar <- coordChoice
  string ", "
  sndCoordChar <- coordChoice
  string "], \"v\", \""
  mark <- choice [char 'O', char 'o', char 'X', char 'x']
  string "\", \"id\", \""
  id <- many letter
  string "\""
  return $ Move (digitToInt fstCoordChar) (digitToInt sndCoordChar) id (toUpper mark)

parseMoves :: Parser [Move]
parseMoves = do
  m <- parseMove
  prevMoveExists <- try (string ", \"prev\", ") <|> (string "")
  if prevMoveExists == ", \"prev\", "
    then do 
         prevMoves <- parseMoves
         return (m:prevMoves)
    else return [m]

readExpr :: String -> Either String [Move]
readExpr input = case parse parseMoves "(none)" input of
    Left err -> Left $ show err
    Right r ->  Right r


coordChoice :: Stream s m Char => ParsecT s u m Char
coordChoice = choice [char '0', char '1', char '2']