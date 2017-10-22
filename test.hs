import Data.List
import Data.Char
import Debug.Trace
import Text.Read

data Move = Move {
    x :: Int,
    y :: Int,
    pId :: String,
    mark :: Char
} deriving Show
instance Eq Move where
    (Move x1 y1 id1 mark1) == (Move x2 y2 id2 mark2) =
        (x1 == x2) && (y1 == y2)

message :: String
--message = "d1:cd1:0i0e1:1i2ee2:id3:vWk4:prevd1:cd1:0i1e1:1i1ee2:id3:vWk1:v1:oe1:v1:xe"
message = "d1:cd1:0i0e1:1i0ee2:id27:PhrgptTJSaeGHSkfOtPottPNrye4:prevd1:cd1:0i2e1:1i2ee2:id13:WkAKUAmstcBHD4:prevd1:cd1:0i0e1:1i1ee2:id27:PhrgptTJSaeGHSkfOtPottPNrye4:prevd1:cd1:0i1e1:1i1ee2:id13:WkAKUAmstcBHD4:prevd1:cd1:0i1e1:1i0ee2:id27:PhrgptTJSaeGHSkfOtPottPNrye4:prevd1:cd1:0i2e1:1i2ee2:id13:WkAKUAmstcBHD1:v1:oe1:v1:xe1:v1:oe1:v1:xe1:v1:oe1:v1:xe"
--(0, 2) = X by "vWk"
--(1, 1) = O by "vWk"

-- message = "d1:cd1:0i2e1:1i1ee2:id4:pAtK1:v1:xe"
-- (2, 1) = X by "pAtK"

--winner :: String -> Either String (Maybe String)
--winner str = Left (Just str)
--winner ('e':str) = Right (Just str)

--ID gali but tuscias string!

go :: String -> [Move]
go "de" = []
go str = parseMoves [] str

parseMoves :: [Move] -> String -> [Move]
parseMoves moves [] = moves
parseMoves moves rest = 
    let 
        (move, rest1) = parseMove rest
    in
        parseMoves (move:moves) rest1
        
parseMove :: String -> (Move, String)
parseMove ('d':rest) = 
    let
        (x, y, restXY) = readCoords rest
        (id, restId) = readId restXY
        (mark, nextMoveString) = readMark restId
    in 
        (Move x y id mark, nextMoveString)

readCoords :: String -> (Int, Int, String)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'0':'e':'1':':':'1':'i':'0':'e':'e':rest) = (0,0, rest)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'0':'e':'1':':':'1':'i':'1':'e':'e':rest) = (0,1, rest)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'0':'e':'1':':':'1':'i':'2':'e':'e':rest) = (0,2, rest)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'1':'e':'1':':':'1':'i':'0':'e':'e':rest) = (1,0, rest)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'1':'e':'1':':':'1':'i':'1':'e':'e':rest) = (1,1, rest)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'1':'e':'1':':':'1':'i':'2':'e':'e':rest) = (1,2, rest)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'2':'e':'1':':':'1':'i':'0':'e':'e':rest) = (2,0, rest)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'2':'e':'1':':':'1':'i':'1':'e':'e':rest) = (2,1, rest)
readCoords ('1':':':'c':'d':'1':':':'0':'i':'2':'e':'1':':':'1':'i':'2':'e':'e':rest) = (2,2, rest)
readCoords _ = error "Wrong X and Y format"

--TODO: Read f-ion validation missing!
readId :: String -> (String, String)
readId ('2':':':'i':'d':rest) = 
    let
        (idLengthStr, restIdLength) = getIdLengthAsString [] rest
        idLength = read idLengthStr
    in 
        getId [] idLength restIdLength

getIdLengthAsString :: String -> String -> (String, String)
getIdLengthAsString str (':':rest) = (str, rest)
getIdLengthAsString str rest = getIdLengthAsString (str ++ [head rest]) (tail rest)

getId :: String -> Int -> String -> (String, String)
getId id 0 rest = (id, rest)
getId id length rest = getId (id ++ [head rest]) (length-1) (tail rest)

--TODO: something smarter for second part of the f-ion?
readMark :: String -> (Char, String)
readMark ('4':':':'p':'r':'e':'v':rest) = getMark (reverse rest)
readMark ('1':rest) = getMark (reverse ('1':rest))

getMark :: String -> (Char, String)
getMark ('e':'x':':':'1':'v':':':'1':rest) = ('X', reverse rest)
getMark ('e':'X':':':'1':'v':':':'1':rest) = ('X', reverse rest)
getMark ('e':'o':':':'1':'v':':':'1':rest) = ('O', reverse rest)
getMark ('e':'O':':':'1':'v':':':'1':rest) = ('O', reverse rest)

areMovesUnique :: [Move] -> Bool
areMovesUnique [] = True
areMovesUnique (m:moves) = m `notElem` moves && areMovesUnique moves

getPlayersMoves :: Char -> [Move] -> ([Move], [Move])
getPlayersMoves char moves = partition (\move -> mark move == char) moves

getWinner :: [Move] -> String
getWinner moves = 
    let 
        (xMoves, oMoves) = getPlayersMoves 'X' moves
        xWon = (findThreeInColumns 0 xMoves) || (findThreeInRows 0 xMoves) || (findThreeDiagonally xMoves)
        oWon = (findThreeInColumns 0 oMoves) || (findThreeInRows 0 oMoves) || (findThreeDiagonally oMoves)
    in
        if xWon then pId (head xMoves)
        else if oWon then pId (head oMoves)
        else "Nobody won"
        -- (xWonColumns, xWonRows, xWonDiagonal, oWonColumns, oWonRows, oWonDiagonal)

findThreeInColumns :: Int -> [Move] -> Bool
findThreeInColumns 3  _ = False
findThreeInColumns counter moves = (length (filter (\move -> x move == counter) moves) == 3) || findThreeInColumns (counter+1) moves

findThreeInRows :: Int -> [Move] -> Bool
findThreeInRows 3  _ = False
findThreeInRows counter moves = (length (filter (\move -> y move == counter) moves) == 3) || findThreeInRows (counter+1) moves

findThreeDiagonally :: [Move] -> Bool
findThreeDiagonally moves = 
    let
        center = find (\move -> x move == 1 && y move == 1) moves
        topLeft = find (\move -> x move == 0 && y move == 0) moves
        topRight = find (\move -> x move == 2 && y move == 0) moves
        botLeft = find (\move -> x move == 0 && y move == 2) moves
        botRight = find (\move -> x move == 2 && y move == 2) moves
        fm Nothing = False
        fm _ = True
    in
        (fm center && fm botLeft && fm topRight) || (fm center && fm topLeft && fm botRight) 

s :: String -> String
s str = getWinner $ go str