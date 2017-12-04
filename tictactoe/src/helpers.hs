module Helpers where

import Happstack.Server
import Control.Monad
import Debug.Trace

import MoveDataType

eitherToIO :: Either String b -> IO b
eitherToIO (Left a) = error a
eitherToIO (Right b) = return b

fromRight :: Either String b -> b
fromRight (Left str)  = error str
fromRight (Right x) = x

traceReceivedMoves :: Monad f => [Move] -> f ()
traceReceivedMoves moves = do
    traceM "POST. Received moves: " 
    traceShowM moves
    traceM ""