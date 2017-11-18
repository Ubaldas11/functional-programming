{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import Network.HTTP as HTTP
import Network.Stream
import Debug.Trace

someFunc :: IO ()
someFunc = do
    getResult <- simpleHTTP (getRequest "http://hackage.haskell.org/")
    let parsedBody = parseGetStr getResult
    putStrLn parsedBody

parseGetStr :: Result (Response String) -> String
parseGetStr (Right value) = rspBody value
parseGetStr (Left _) = "Conn error occured"

--simpleHTTP :: HStream ty => Request ty -> IO (Result (Response ty))
