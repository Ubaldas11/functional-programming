module MinimaxTests where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Move
import MoveDataType

minimaxTests = testGroup "Minimax tests" [ firstAttackMove, attackMirrorsMove, gameScoreDraw, gameScoreDefWon, gameScoreDefLost, newBoardsGenerated ]


firstAttackMove :: TestTree
firstAttackMove = testCase "First attacking move is center" (makeMove gameBoard1 attId attMark @?= expectedMove1)
gameBoard1 = []
expectedMove1 = Move 1 1 attId attMark

attackMirrorsMove :: TestTree
attackMirrorsMove = testCase "Attacker mirrors opponent's last move" (makeMove gameBoard2 attId attMark @?= expectedMove2)
gameBoard2 = [ Move 1 1 attId attMark, Move 0 0 defId defMark]
expectedMove2 = Move 2 2 attId attMark

gameScoreDraw :: TestTree
gameScoreDraw = testCase "Draw game score is 0" (gameScore gameBoard3 defMark @?= 0)
gameBoard3 = [ Move 0 0 attId attMark, Move 1 0 attId attMark, Move 2 0 defId defMark,
               Move 0 1 defId defMark, Move 1 1 defId defMark, Move 2 1 attId attMark,
               Move 0 2 attId attMark, Move 1 2 defId defMark, Move 2 2 attId attMark]

gameScoreDefWon :: TestTree
gameScoreDefWon = testCase "Defender won game score is 10" (gameScore gameBoard4 defMark @?= 10)
gameBoard4 = [ Move 0 0 attId attMark, Move 1 0 defId defMark, Move 2 0 defId defMark,
               Move 0 1 defId defMark, Move 1 1 attId attMark, Move 2 1 attId attMark,
               Move 0 2 attId attMark, Move 1 2 defId defMark, Move 2 2 attId attMark]

gameScoreDefLost :: TestTree
gameScoreDefLost = testCase "Defender lost game score is -10" (gameScore gameBoard5 defMark @?= (-10))
gameBoard5 = [ Move 0 0 defId defMark,                         Move 2 0 attId attMark,
               Move 0 1 defId defMark, Move 1 1 attId attMark, 
               Move 0 2 defId defMark, Move 1 2 attId attMark                         ]

newBoardsGenerated :: TestTree
newBoardsGenerated = testCase "New 3 possible boards generated" (assertBool "Expected and generated boards differ" (null (generatedBoards \\ expectedBoards)))
gameBoard6 = [ Move 0 0 attId attMark, Move 1 0 defId defMark, Move 2 0 attId attMark,
               Move 0 1 defId defMark, Move 1 1 attId attMark, Move 2 1 defId defMark]
possibleMoves = board \\ gameBoard6
generatedBoards = getNewGames attMark gameBoard6 possibleMoves []
expectedBoards = [ gameBoard6 ++ [Move 0 2 "E" attMark], gameBoard6 ++ [Move 1 2 "E" attMark], gameBoard6 ++ [Move 2 2 "E" attMark]]

attId = "Attacker"
attMark = 'X'
defId = "Defender"
defMark = 'O'