{-# LANGUAGE OverloadedStrings #-}
module Server where 

-- import Data.ByteString.Lazy.Char8 hiding (length)
-- import Control.Monad
-- import Control.Monad.IO.Class (liftIO)
-- import Happstack.Server
-- import Data.Maybe
-- import Data.List.Split
-- import Debug.Trace

-- import SmartParser
-- import Parser
-- import Helpers
-- import Lib

-- startServer :: IO ()
-- startServer = do
--     traceIO "Web server started."
--     simpleHTTP nullConf handlers

-- standardPolicy :: BodyPolicy
-- standardPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

-- handlers :: ServerPart Response
-- handlers = do 
--     decodeBody standardPolicy
--     msum [ dirs "game/" $ gameBoardPost
--          , dir "history" $ historyGet ]

-- getBody :: ServerPart ByteString
-- getBody = do
--     req  <- askRq 
--     body <- liftIO $ takeRequestBody req
--     return $ unBody $ fromJust $ body

-- gameBoardPost :: ServerPart Response
-- gameBoardPost = do
--   noTrailingSlash
--   method POST
--   rq <- askRq
--   let parts = splitOn "/" (rqUri rq)
--   --when (length parts /= 3) (badRequest $ toResponse badUrlString)
--   body <- getBody
--   let moves = fromRight $ parseStrToMoves $ unpack body
--   traceReceivedMoves moves
--   let nextBoard = getNextBoard moves
--   case nextBoard of
--     Left str -> do
--         let msg = "Received game is over. Result: \n" ++ str
--         traceM msg
--         ok $ toResponse msg
--     Right val -> do
--         let postResponse = sendPostRequest val "http://tictactoe.haskell.lt/game/15935/player/1"
--         ok $ toResponse val


-- historyGet :: ServerPart Response
-- historyGet = do
--     method GET
--     body <- getBody
--     ok $ toResponse gameHistory

-- gameHistory :: String
-- gameHistory = "History should be here"

-- badUrlString :: String
-- badUrlString = "Bad URL."