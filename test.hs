import Data.List
import Data.Char
import Debug.Trace

data Move = Move {
    x :: Int,
    y :: Int,
    pId :: String,
    mark :: Char
} deriving Show

message = "d1:cd1:0i0e1:1i0ee2:id27"

instance Eq Move where
    (Move x1 y1 id1 mark1) == (Move x2 y2 id2 mark2) =
        (x1 == x2) && (y1 == y2)

w :: String -> String
w "de" = "The board is empty"
w str = 
    let 
        moves = parseMoves [] str
    in 
        if (areMovesUnique moves) 
        then getWinner moves
        else "There are overlapping moves"        

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
parseMove _ = error "Wrong start of move message"

-- VALIDATE COORDS INPUT
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

--TODO: read should be rewritten to readMaybe
readId :: String -> (String, String)
readId ('2':':':'i':'d':rest) = 
    let
        (idLengthStr, restIdLength) = getIdLengthAsString [] rest
        idLength = parseNumber 0 1 idLengthStr
    in 
        getId [] idLength restIdLength
readId _ = error "Wrong id format"

--Catch empty string after colon
getIdLengthAsString :: String -> String -> (String, String)
getIdLengthAsString str [] = error "String is empty after ID length"
getIdLengthAsString str (':':rest) = (str, rest)
getIdLengthAsString str rest = getIdLengthAsString (str ++ [head rest]) (tail rest)

getId :: String -> Int -> String -> (String, String)
getId id 0 rest = (id, rest)
getId id length rest = getId (id ++ [head rest]) (length-1) (tail rest)

--TODO: something smarter for second part of the f-ion?
--catch wrong format
readMark :: String -> (Char, String)
readMark ('4':':':'p':'r':'e':'v':rest) = getMark (reverse rest)
readMark ('1':rest) = getMark (reverse ('1':rest))
readMark _ = error "Wrong format after ID"

getMark :: String -> (Char, String)
getMark ('e':'x':':':'1':'v':':':'1':rest) = ('X', reverse rest)
getMark ('e':'X':':':'1':'v':':':'1':rest) = ('X', reverse rest)
getMark ('e':'o':':':'1':'v':':':'1':rest) = ('O', reverse rest)
getMark ('e':'O':':':'1':'v':':':'1':rest) = ('O', reverse rest)
getMark _ = error "Wrong mark format"

areMovesUnique :: [Move] -> Bool
areMovesUnique [] = True
areMovesUnique (m:moves) = m `notElem` moves && areMovesUnique moves

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

getPlayersMoves :: Char -> [Move] -> ([Move], [Move])
getPlayersMoves char moves = partition (\move -> mark move == char) moves

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

parseNumber :: Int -> Int -> String -> Int
parseNumber 0 1 [] = error "Id length is missing"
parseNumber num i [] = num
parseNumber num i rest =
    let
        digit = last rest
    in
        case digit of '1' -> parseNumber (num + 1 * i) (i*10) (init rest)
                      '2' -> parseNumber (num + 2 * i) (i*10) (init rest)
                      '3' -> parseNumber (num + 3 * i) (i*10) (init rest)
                      '4' -> parseNumber (num + 4 * i) (i*10) (init rest)
                      '5' -> parseNumber (num + 5 * i) (i*10) (init rest)
                      '6' -> parseNumber (num + 6 * i) (i*10) (init rest)
                      '7' -> parseNumber (num + 7 * i) (i*10) (init rest)
                      '8' -> parseNumber (num + 8 * i) (i*10) (init rest)
                      '9' -> parseNumber (num + 9 * i) (i*10) (init rest)
                      _ -> error "Id length is not a number"