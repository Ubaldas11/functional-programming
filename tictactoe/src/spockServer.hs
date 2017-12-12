{-# LANGUAGE OverloadedStrings #-}
module SpockServer where

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
import Lib
import Move

data MySession = EmptySession
data MyAppState = DummyAppState (IORef [(String, Bool)])

startSpockServer :: IO ()
startSpockServer =
    do ref <- newIORef []
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8000 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $
        text "Hello World!"
    get ("history") $ do
        (DummyAppState ref) <- getState
        gameIds <- liftIO $ readIORef ref
        text (T.pack (show gameIds))
    post ("game" <//> var) $ \gameId -> do
        (DummyAppState ref) <- getState
        history <- liftIO $ readIORef ref
        let currGame = find (\g -> fst g == gameId) history
        when (isJust currGame && snd (fromJust currGame) == True) (text "Game has ended")
        encodedBoard <- body
        let moves = fromRight $ parseStrToMoves (B.unpack encodedBoard)
        traceReceivedMoves moves
        let nextBoard = getNextBoard moves
        case nextBoard of
            Left msg -> text (T.pack (show msg))
            Right val -> do
                let gameFinished = isGameOverStr val
                traceShowM gameFinished
                liftIO $ atomicModifyIORef' ref $ updateState gameFinished gameId history
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

replaceAtIndex :: Int -> x -> [x] -> [x]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls
                   