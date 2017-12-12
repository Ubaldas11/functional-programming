module Client where

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
    let url = "http://127.0.0.1:8000/game/" ++ gameId
    let playerId = "Client"
    let firstMove = getNewBoardStr [] playerId
    play firstMove url playerId

play :: String -> String -> String -> IO ()
play board url playerId = do
    postResponse <- sendPostRequest board url
    moves <- getValidMoves postResponse
    let movesStr = getNewBoardStr moves playerId
    play movesStr url playerId

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
    traceIO "\n"
    traceIO ("Sending POST to " ++ url)
    let postRequest = postRequestWithBody url "application/bencode+map" board
    postResponseRes <- simpleHTTP postRequest
    rspBody <- eitherToIO $ parseRspBody postResponseRes
    rspCode <- getResponseCode postResponseRes
    traceIO "POST response:"
    traceIO rspBody
    let gameOver = isGameOverStr board
    when gameOver exitSuccess
    when (rspCode /= (2, 0, 0)) (error $ "Response code is " ++ show rspCode ++ ". Stopping.")
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

baseUrl :: String
baseUrl = "http://tictactoe.haskell.lt/game/"