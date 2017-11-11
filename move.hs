import Parser
import MoveDataType

move :: String -> Either String Char
move str = do
  moves <- parseMoves [] str
  mark <- myMark moves
  return mark

move1 :: String -> Either String [Move]
move1 str = do
  moves <- parseMoves [] str
  mark <- myMark moves
  return moves

myMark :: [Move] -> Either String Char
myMark moves = if mark (last moves) == 'X' then Right 'O' else Right 'X'