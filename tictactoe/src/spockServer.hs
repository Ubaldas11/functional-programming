{-# LANGUAGE OverloadedStrings #-}
module SpockServer where

import Network.HTTP.Types.Status
import Web.Spock
import Web.Spock.Config
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import Data.Maybe
import Data.List hiding (insert)
import Data.Set as Set
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
data MyAppState = GameHistory (IORef (Set (String, Bool)))

startSpockServer :: IO ()
startSpockServer =
    do ref <- newIORef empty
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (GameHistory ref)
       runSpock 8000 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $
        text "Hello World!"
    get ("history") $ do
        (GameHistory ref) <- getState
        gameIds <- liftIO $ readIORef ref
        let finishedGames = Set.filter (\g -> snd g == True) gameIds
        let finishedGameIds = elems $ Set.map (\g -> fst g) finishedGames
        let unfGames = Set.filter (\g -> notMember ((fst g, True)) gameIds) gameIds
        let unfGameIds = elems $ Set.map (\g -> fst g) unfGames
        text (T.pack ("Finished games: " ++ show finishedGameIds ++ "\nUnfinished games: " ++ show unfGameIds ++ "\nState: " ++ show gameIds))
    post ("game" <//> var) $ \gameId -> do
        (GameHistory ref) <- getState
        history <- liftIO $ readIORef ref
        when (member (gameId, True) history) (setStatus status418 >> text "Game has ended")
        liftIO $ atomicModifyIORef' ref $ \i -> (insert (gameId, False) i, insert (gameId, False) i)
        encodedBoard <- body
        let moves = fromRight $ parseStrToMoves (B.unpack encodedBoard)
        traceReceivedMoves moves
        let nextBoard = getNextBoard moves
        case nextBoard of
            Left msg -> do
                liftIO $ atomicModifyIORef' ref $ \i -> (insert (gameId, True) i, insert (gameId, True) i)
                text (T.pack (show msg))
            Right val -> do
                text (T.pack val)


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
                   