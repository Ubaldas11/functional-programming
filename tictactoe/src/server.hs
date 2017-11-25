{-# LANGUAGE OverloadedStrings #-}
module Server where 

import Data.ByteString.Lazy.Char8
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import Data.Maybe

startServer :: IO ()
startServer = simpleHTTP nullConf handlers

standardPolicy :: BodyPolicy
standardPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

handlers :: ServerPart Response
handlers = do 
    decodeBody standardPolicy
    msum [ dir "game" $ gameBoardPost
         , dir "history" $ historyGet ]

getBody :: ServerPart ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req
    return $ unBody $ fromJust $ body

gameBoardPost :: ServerPart Response
gameBoardPost = do
  method POST
  body <- getBody
  ok $ toResponse body

historyGet :: ServerPart Response
historyGet = do
    body <- getBody
    ok $ toResponse gameHistory

gameHistory :: String
gameHistory = "History should be here"