module ValidatorTests where

import Test.Tasty
import Test.Tasty.HUnit

import MoveDataType
import Validator

validatorTests = testGroup "Validator tests" [overlappingMoves, incorrectMoveOrder]

overlappingMoves :: TestTree
overlappingMoves = testCase "Overlapping moves detected" (areMovesUnique notUniqueMoves @?= False)
notUniqueMoves = [ Move 0 0 attId attMark, Move 1 0 defId defMark, Move 2 0 attId attMark,
                   Move 1 1 defId defMark, Move 1 1 attId attMark, Move 2 1 defId defMark]

incorrectMoveOrder :: TestTree
incorrectMoveOrder = testCase "Incorrect move order detected" (correctMoveOrder 'X' twoOsInARowMoves @?= False)
twoOsInARowMoves = [ Move 0 0 attId attMark, Move 1 0 defId defMark, Move 2 0 attId defMark,
                     Move 1 1 defId attMark, Move 1 1 attId defMark, Move 2 1 defId attMark]


attId = "Attacker"
attMark = 'X'
defId = "Defender"
defMark = 'O'