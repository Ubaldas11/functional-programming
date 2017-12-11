{-# LANGUAGE OverloadedStrings #-}
module SpockServer where

import Web.Spock
import Web.Spock.Config
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import Data.Sequence
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

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
        encodedBoard <- body
        let moves = fromRight $ parseStrToMoves (B.unpack encodedBoard)
        traceReceivedMoves moves
        let nextBoard = getNextBoard moves
        case nextBoard of
            Left msg -> text (T.pack (show msg))
            Right val -> do
                let gameFinished = isGameOverStr val
                liftIO $ atomicModifyIORef' ref $ updateState gameId history
                text (T.pack val)
                
updateState :: String -> [(String, Bool)] -> [(String, Bool)] -> ([(String, Bool)], [(String, Bool)])
updateState gameId history boards = 
    let
        gameExists = lookup gameId history
    in
        case gameExists of
            Nothing -> (history ++ [(gameId, False)], history ++ [(gameId, False)])
            Just val ->
                let
                    itemToUpdateIndex = fromJust $ lookupIndex 

replaceNth :: Int -> a -> [a] 
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs
                   