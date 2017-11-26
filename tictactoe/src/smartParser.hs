{-# LANGUAGE FlexibleContexts #-}
module SmartParser where

import Text.Parsec hiding (runParser)
import Text.ParserCombinators.Parsec hiding (try)
import MoveDataType
import Data.Char

message = "[\"c\", [1, 0], \"v\", \"x\", \"id\", \"BNusyMdbcWtWjYfBrUxe\", \"prev\", [\"c\", [0, 1], \"v\", \"o\", \"id\", \"rqFsvhTQktfAjQwQRXIi\", \"prev\", [\"c\", [0, 0], \"v\", \"x\", \"id\", \"BNusyMdbcWtWjYfBrUxe\", \"prev\", [\"c\", [2, 2], \"v\", \"o\", \"id\", \"rqFsvhTQktfAjQwQRXIi\", \"prev\", [\"c\", [0, 2], \"v\", \"x\", \"id\", \"BNusyMdbcWtWjYfBrUxe\", \"prev\", [\"c\", [1, 2], \"v\", \"o\", \"id\", \"rqFsvhTQktfAjQwQRXIi\", \"prev\", [\"c\", [1, 1], \"v\", \"x\", \"id\", \"BNusyMdbcWtWjYfBrUxe\", \"prev\", [\"c\", [2, 0], \"v\", \"o\", \"id\", \"rqFsvhTQktfAjQwQRXIi\", \"prev\", [\"c\", [2, 1], \"v\", \"x\", \"id\", \"BNusyMdbcWtWjYfBrUxe\"]]]]]]]]]"

parseMove :: Parsec String Int Move
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

parseMoves :: Parsec String Int [Move]
parseMoves = do
  m <- parseMove
  continue <- try (string ",") <|> (checkEndBrackets)
  if continue == ","
    then do
         modifyState (+1)
         string " \"prev\", "
         prevMoves <- parseMoves
         return (m:prevMoves)
    else do
         notFollowedBy anyChar 
         return [m]

parseJsonMoves :: String -> Either String [Move]
parseJsonMoves "[]" = Right []
parseJsonMoves input = case runParser parseMoves 1 "" input of
    Left err -> Left $ show err
    Right r ->  Right r

coordChoice :: Stream s m Char => ParsecT s Int m Char
coordChoice = choice [char '0', char '1', char '2']

checkEndBrackets :: Stream s m Char => ParsecT s Int m String
checkEndBrackets = do
  c <- getState
  count c $ char ']'