module QuickCheckTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Move
import MoveDataType

import Data.List
import Data.Ord

quickCheckTests = testGroup "Quickcheck tests" [ qcProps ]

qcProps = testGroup "Minimax quickcheck"
  [ QC.testProperty "Attacker's move is mirrored" $
   \m -> propMirrorBoard m
    --   \list -> sort (list :: [Int]) == sort (reverse list)
  ]

instance Arbitrary Move where
    arbitrary = 

propMirrorBoard :: Move  -> Bool
propMirrorBoard defMove = 
    let
        gameBoard = [ Move 1 1 attId attMark, defMove]
        nextMove = makeMove gameBoard attId attMark
    in
        isCoordMirrored (x defMove) (x nextMove) && isCoordMirrored (y defMove) (y nextMove)

isCoordMirrored :: Int -> Int -> Bool
isCoordMirrored x1 x2 = 
    if ((x1 == 1 && x2 == 1) || (x1 == 0 && x2 == 2) || (x1 == 2 && x2 == 0))
        then True
        else False

attId = "ATT"
attMark = 'X'
defId = "DEF"
defMark = 'O'