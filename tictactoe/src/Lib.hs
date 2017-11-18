{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import Network.HTTP
import Network.Stream
import Network.URI
import Debug.Trace
import Control.Monad
import Data.Either
import Data.Maybe
import System.Environment

import Move
import MoveDataType
import Parser
import Validator

someFunc :: IO ()
someFunc = do
    (gameId, playerId) <- liftM parseArgs getArgs
    if playerId == "1"
        then attack gameId playerId "de"
        else defend gameId playerId
    

baseUrl :: String
baseUrl = "http://tictactoe.haskell.lt/game/"

defend :: String -> String -> IO ()
defend gameId playerId = do
    let url = baseUrl ++ gameId ++ "/player/" ++ playerId
    getResponse <- sendGetRequest url
    when (isLeft getResponse) (putStrLn (fromLeft getResponse))
    let boardStr = fromRight getResponse
    moves <- getValidMoves boardStr
    let newBoardStr = getNewBoardStr moves playerId
    postResponse <- sendPostRequest newBoardStr url
    when (isLeft postResponse) (putStrLn (fromLeft postResponse))
    defend gameId playerId

attack :: String -> String -> String -> IO ()
attack gameId playerId boardStr = do
    let url = baseUrl ++ gameId ++ "/player/" ++ playerId
    moves <- getValidMoves boardStr
    let boardStr = getNewBoardStr moves playerId
    postResponse <- sendPostRequest boardStr url
    when (isLeft postResponse) (putStrLn (fromLeft postResponse))
    getResponse <- sendGetRequest url
    when (isLeft getResponse) (putStrLn (fromLeft getResponse))
    let newBoardStr = fromRight getResponse
    attack gameId playerId newBoardStr

getValidMoves :: String -> IO [Move]
getValidMoves str = do 
    let moves = parseMoves [] str
    when (isLeft moves) (putStrLn (fromLeft moves))
    let actualMoves = fromRight moves
    let validBoard = validate actualMoves
    when (isLeft validBoard) (putStrLn (fromLeft validBoard))
    let gameOver = isGameOver actualMoves
    when (gameOver) (putStrLn "Game Over")
    return actualMoves

getNewBoardStr :: [Move] -> String -> String
getNewBoardStr moves id = 
    let
        myMark = getMyMark moves
        newBoard = getBoardWithMove moves id myMark
        newBoardStr = convertMoves newBoard ""
    in
        newBoardStr

sendPostRequest :: String -> String -> IO (Either String String)
sendPostRequest board url = do
    let postRequest = postRequestWithBody url "application/bencode+map" board
    postResponse <- simpleHTTP postRequest
    traceIO (show postResponse)
    traceIO (show (parseResponseToStr postResponse))
    traceIO ("-----")
    return (parseResponseToStr postResponse)

sendGetRequest :: String -> IO (Either String String)
sendGetRequest url = do
    let request = getRequest url
    let reqWithHead = setHeaders request [mkHeader HdrAccept "application/bencode+map" ]
    getResponse <- simpleHTTP reqWithHead
    traceIO (show getResponse)
    traceIO (show (parseResponseToStr getResponse))
    traceIO ("-----")
    return (parseResponseToStr getResponse)

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _ = error "Value is right"

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Value is left"

parseResponseToStr :: Result (Response String) -> Either String String
parseResponseToStr (Right value) = Right (rspBody value)
parseResponseToStr (Left _) = Left "Connection error"

parseArgs :: [String] -> (String, String)
parseArgs (gameId:"1":[]) = (gameId, "1")
parseArgs (gameId:"2":[]) = (gameId, "2")
parseArgs _ = error "Wrong command line input"
