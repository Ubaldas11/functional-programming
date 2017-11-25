{-# LANGUAGE OverloadedStrings #-}
module Server where 

import qualified Data.ByteString.Lazy.Char8 as L    
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Happstack.Server

startServer :: IO ()
startServer = simpleHTTP nullConf handlers

-- startServer = simpleHTTP nullConf $ msum 
--     [ do dir "foo" $ do method GET
--                         ok $ "GET request on /foo"
--     , do method POST 
--          ok $ "Post success"
--     ]

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

handlers :: ServerPart Response
handlers = do 
    decodeBody myPolicy
    msum [ dir "game" $ postUnit ]

getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return ""

postUnit :: ServerPart Response
postUnit = do
  body <- getBody
  ok $ toResponse body