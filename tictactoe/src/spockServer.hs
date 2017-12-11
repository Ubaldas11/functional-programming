{-# LANGUAGE OverloadedStrings #-}
module SpockServer where

import Web.Spock
import Web.Spock.Config
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import SmartParser
import Parser
import Helpers
import Lib

data MySession = EmptySession
data MyAppState = DummyAppState (IORef [String])

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
        encodedBoard <- body
        let moves = fromRight $ parseStrToMoves (B.unpack encodedBoard)
        traceReceivedMoves moves
        let nextBoard = getNextBoard moves
        case nextBoard of
            Left msg -> text (T.pack (show msg))
            Right val -> do
                liftIO $ atomicModifyIORef' ref $ \currState -> (currState ++ [gameId], currState ++ [gameId])
                text (T.pack (show val)) 
                   