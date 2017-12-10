module Lib where

import Network.HTTP
import Network.Stream
import Network.URI
import Debug.Trace
import Control.Monad
import Data.Either
import Data.Maybe
import System.Environment
import System.Exit

import Move
import MoveDataType
import Parser
import Validator
import Encoder
import Helpers

startClient :: IO ()
startClient = do
    gameId <- liftM parseArgs getArgs
    let url = "localhost:8000/game/" ++ gameId
    let playerId = "Client"
    let firstMove = getNewBoardStr [] playerId
    play firstMove url playerId

play :: String -> String -> String -> IO ()
play board url playerId = do
    postResponse <- sendPostRequest board url
    moves <- getValidMoves postResponse
    let movesStr = getNewBoardStr moves playerId
    play movesStr url playerId

-- Get parsed moves and return 
getNextBoard :: [Move] -> Either String String
getNextBoard moves = do
    gameNotOver <- shouldGameContinue moves
    let newBoard = getNewBoardStr moves "Ubaldas"
    return newBoard
    
baseUrl :: String
baseUrl = "http://tictactoe.haskell.lt/game/"

defend :: String -> String -> String -> IO ()
defend url gameId playerId = do
    boardStr <- sendGetRequest url
    moves <- getValidMoves boardStr
    let newBoardStr = getNewBoardStr moves playerId
    postResponse <- sendPostRequest newBoardStr url
    defend url gameId playerId

attack :: String -> String -> String -> String -> IO ()
attack url gameId playerId boardStr = do
    moves <- getValidMoves boardStr
    let boardStr = getNewBoardStr moves playerId
    postResponse <- sendPostRequest boardStr url
    newBoardStr <- sendGetRequest url
    attack url gameId playerId newBoardStr

getValidMoves :: String -> IO [Move]
getValidMoves str = do 
    moves <- eitherToIO $ parseStrToMoves str
    validBoard <- eitherToIO $ validate moves
    let gameOver = isGameOver moves
    when (gameOver) (exitWithSuccess "Game Over")
    return moves

getNewBoardStr :: [Move] -> String -> String
getNewBoardStr moves id = 
    let
        myMark = getMyMark moves
        newBoard = getBoardWithMove moves id myMark
        newBoardStr = encMovesToStr newBoard
    in
        newBoardStr

sendPostRequest :: String -> String -> IO String
sendPostRequest board url = do
    traceIO "Got into POST send."
    let postRequest = postRequestWithBody url "application/bencode+map" board
    postResponseRes <- simpleHTTP postRequest
    traceIO $ show postResponseRes
    rspBody <- eitherToIO $ parseRspBody postResponseRes
    let gameOver = isGameOverStr board
    when gameOver (exitWithSuccess "Game Over")
    return rspBody

sendGetRequest :: String -> IO String
sendGetRequest url = do
    let request = getRequest url
    let reqWithHead = setHeaders request [mkHeader HdrAccept "application/bencode+map" ]
    getResponseRes <- simpleHTTP reqWithHead
    traceIO $ show getResponseRes
    rspBody <- eitherToIO $ parseRspBody getResponseRes
    return rspBody

parseRspBody :: Result (Response String) -> Either String String
parseRspBody (Right value) = Right (rspBody value)
parseRspBody (Left _) = Left "Connection error"

parseArgs :: [String] -> String
parseArgs (gameId:[]) = gameId
parseArgs _ = error "Wrong command line input"

exitWithSuccess :: String -> IO ()
exitWithSuccess msg = (putStrLn msg) >> exitSuccess

