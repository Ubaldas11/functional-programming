module QuickCheckTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Move
import MoveDataType

quickCheckTests = testGroup "Quickcheck tests" [ qcProps ]

qcProps = testGroup "Minimax quickcheck"
  [ QC.testProperty "Attacker mirrors opponent's move" $
   \m -> propMirrorBoard m
  ]

propMirrorBoard :: Move  -> Bool
propMirrorBoard defMove = 
    let
        attMark = swapMark (mark defMove)
        gameBoard = [ Move 1 1 attId attMark, defMove]
        nextMove = makeMove gameBoard attId attMark
    in
        
        (x defMove == 1 && y defMove == 1) || -- generator can give 1 1 coords, which is already taken
        isCoordMirrored (x defMove) (x nextMove) && isCoordMirrored (y defMove) (y nextMove)

isCoordMirrored :: Int -> Int -> Bool
isCoordMirrored x1 x2 = (x1 == 1 && x2 == 1) || (x1 == 0 && x2 == 2) || (x1 == 2 && x2 == 0)

attId = "ATT"