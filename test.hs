import Data.List
import Data.Char
import Debug.Trace

data Move = Move {
    x :: Int,
    y :: Int,
    pId :: String,
    mark :: Char
} deriving Show

message = "d1:cccd1:0i1e1:1i0ee2:id14:TwqkxOFmwqdSCK4:prevd1:cd1:0i2e1:1i0ee2:id2:IY4:prevd1:cd1:0i2e1:1i1ee2:id14:TwqkxOFmwqdSCK4:prevd1:cd1:0i1e1:1i2ee2:id2:IY4:prevd1:cd1:0i2e1:1i2ee2:id14:TwqkxOFmwqdSCK1:v1:oe1:v1:oe1:v1:xe1:v1:oe1:v1:xe"

instance Eq Move where
    (Move x1 y1 id1 mark1) == (Move x2 y2 id2 mark2) =
        (x1 == x2) && (y1 == y2)

winner :: String -> Either String (Maybe String)
winner "de" = Left "ERROR: No moves made"
winner str = 
    let 
        moves = parseMoves [] str
    in 
        case moves of 
                Left msg -> Left msg
                Right moves ->
                                if (areMovesUnique moves) 
                                then Right (getWinner moves)
                                else Left "ERROR: There are overlapping moves"   

             
parseMoves :: [Move] -> String -> Either String [Move]
parseMoves moves [] = Right moves
parseMoves moves rest = 
    let 
        move = parseMove rest
    in
        case move of
            Left msg -> Left msg
            Right move -> parseMoves ((fst move):moves) (snd move)

        
parseMove :: String -> Either String (Move, String)
parseMove ('d':rest) = 
    let
        (x, y, restXY) = readCoords rest
        (id, restId) = readId restXY
        mark = readMark restId
    in 
        case mark of
            Left msg -> Left msg
            Right mark -> Right (Move x y id (fst mark), snd mark)
parseMove _ = Left "ERROR: Wrong start of move message"

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
readCoords _ = (-1, -1, "ERROR: wrong coords format")

readId :: String -> (String, String)
readId ('2':':':'i':'d':rest) = 
    let
        (idLengthStr, restIdLength) = getIdLengthAsString [] rest
        idLength = parseNumber 0 1 idLengthStr
    in 
        case idLength of
            -1 -> ("ERROR: ID length is not a number", "ERROR: ID length is not a number")
            _ -> getId [] idLength restIdLength
readId _ = ("ERROR: wrong ID format", "ERROR: wrong id format")

getIdLengthAsString :: String -> String -> (String, String)
getIdLengthAsString str [] =("ERROR: String is empty after ID length", "ERROR: String is empty after ID length")
getIdLengthAsString str (':':rest) = (str, rest)
getIdLengthAsString str rest = getIdLengthAsString (str ++ [head rest]) (tail rest)

getId :: String -> Int -> String -> (String, String)
getId _ _ [] = ("ERROR: ID is empty", "ERROR: ID is empty")
getId id 0 rest = (id, rest)
getId id length rest = getId (id ++ [head rest]) (length-1) (tail rest)

--TODO: something smarter for second part of the f-ion?
--catch wrong format
readMark :: String -> Either String (Char, String)
readMark ('4':':':'p':'r':'e':'v':rest) = getMark (reverse rest)
readMark ('1':rest) = getMark (reverse ('1':rest))
readMark msg = Left msg

getMark :: String -> Either String (Char, String)
getMark ('e':'x':':':'1':'v':':':'1':rest) = Right ('X', reverse rest)
getMark ('e':'X':':':'1':'v':':':'1':rest) = Right ('X', reverse rest)
getMark ('e':'o':':':'1':'v':':':'1':rest) = Right ('O', reverse rest)
getMark ('e':'O':':':'1':'v':':':'1':rest) = Right ('O', reverse rest)
getMark _ = Left "ERROR: Wrong mark format"

areMovesUnique :: [Move] -> Bool
areMovesUnique [] = True
areMovesUnique (m:moves) = m `notElem` moves && areMovesUnique moves


getWinner :: [Move] -> Maybe String
getWinner moves = 
    let 
        (xMoves, oMoves) = getPlayersMoves 'X' moves
        xWon = (findThreeInColumns 0 xMoves) || (findThreeInRows 0 xMoves) || (findThreeDiagonally xMoves)
        oWon = (findThreeInColumns 0 oMoves) || (findThreeInRows 0 oMoves) || (findThreeDiagonally oMoves)
    in
        case (xWon, oWon) of
            (True, _) -> Just (pId (head xMoves))
            (_, True) -> Just (pId (head oMoves))
            (_, _) -> Nothing

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
parseNumber 0 1 [] = -1 -- ERROR parsing number
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
                      _ -> -1 -- ERROR parsing number