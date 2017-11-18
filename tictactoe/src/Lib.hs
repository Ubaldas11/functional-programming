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

import Move
import MoveDataType
import Parser
import Validator

someFunc :: IO ()
someFunc = do
    attack "de"


attack :: String -> IO ()
attack boardStr = do
    moves <- getValidMoves boardStr
    let boardStr = getNewBoardStr moves "Ubaldas"
    postResponse <- sendPostRequest boardStr "http://tictactoe.haskell.lt/game/veryuniqueid/player/1"
    when (isLeft postResponse) (putStrLn (fromLeft postResponse))
    getResponse <- sendGetRequest "http://tictactoe.haskell.lt/game/veryuniqueid/player/1"
    when (isLeft getResponse) (putStrLn (fromLeft getResponse))
    let newBoardStr = fromRight getResponse
    attack newBoardStr

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
        newBoard = getBoardWithMove moves "Ubaldas" myMark
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
