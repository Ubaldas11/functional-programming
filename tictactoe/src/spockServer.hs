{-# LANGUAGE OverloadedStrings #-}
module SpockServer where

import Network.HTTP.Types.Status
import Web.Spock
import Web.Spock.Config
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Debug.Trace
import Control.Monad

import SmartParser
import Parser
import Helpers
import Move
import MoveDataType
import Encoder

data MySession = EmptySession
data MyAppState = GameHistory (IORef [(String, Bool)])

startSpockServer :: IO ()
startSpockServer =
    do ref <- newIORef []
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (GameHistory ref)
       runSpock 8000 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $
        text "Hello World!"
    get ("history") $ do
        (GameHistory ref) <- getState
        gameIds <- liftIO $ readIORef ref
        text (T.pack (show gameIds))
    post ("game" <//> var) $ \gameId -> do
        (GameHistory ref) <- getState
        history <- liftIO $ readIORef ref
        let currGame = find (\g -> fst g == gameId) history
        when (isJust currGame && snd (fromJust currGame) == True) (setStatus status418 >> text "Game has ended")
        encodedBoard <- body
        let moves = fromRight $ parseStrToMoves (B.unpack encodedBoard)
        traceReceivedMoves moves
        let nextBoard = getNextBoard moves
        case nextBoard of
            Left msg -> do
                liftIO $ atomicModifyIORef' ref $ updateState True gameId history
                text (T.pack (show msg))
            Right val -> do
                liftIO $ atomicModifyIORef' ref $ updateState False gameId history
                text (T.pack val)
                
updateState :: Bool -> String -> [(String, Bool)] -> [(String, Bool)] -> ([(String, Bool)], [(String, Bool)])
updateState gameFinished gameId history boards = 
    let
        game = find (\g -> fst g == gameId) history
    in
        case game of
            Nothing -> (history ++ [(gameId, False)], history ++ [(gameId, False)])
            Just val ->
                let
                    index = fromJust $ elemIndex val history
                    updatedHistory = replaceAtIndex index (fst val, gameFinished) history
                in
                    (updatedHistory, updatedHistory)


getNextBoard :: [Move] -> Either String String
getNextBoard moves = do
    gameOver <- shouldGameContinue moves
    let newBoard = getNewBoardStr moves "Server"
    return newBoard

getNewBoardStr :: [Move] -> String -> String
getNewBoardStr moves id = 
    let
        myMark = getMyMark moves
        newBoard = getBoardWithMove moves id myMark
        newBoardStr = encMovesToStr newBoard
    in
        newBoardStr


replaceAtIndex :: Int -> x -> [x] -> [x]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls
                   