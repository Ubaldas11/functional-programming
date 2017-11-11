module Parser where

import MoveDataType
    
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
readId ('E':'R':'R':rest) = (('E':'R':'R':rest), ('E':'R':'R':rest))
readId ('2':':':'i':'d':rest) = 
    let
        (idLengthStr, restIdLength) = getIdLengthAsString [] rest
        idLength = parseNumber 0 1 idLengthStr
    in 
        case idLength of
            -1 -> ("ERROR: ID length is not a number", "ERROR: ID length is not a number")
            _ -> getId [] idLength restIdLength
readId _ = ("ERROR: wrong ID format", "ERROR: wrong ID format")

getIdLengthAsString :: String -> String -> (String, String)
getIdLengthAsString str [] =("ERROR: String is empty after ID length", "ERROR: String is empty after ID length")
getIdLengthAsString str (':':rest) = (str, rest)
getIdLengthAsString str rest = getIdLengthAsString (str ++ [head rest]) (tail rest)

--catching previous errors is risky as ID might be called like the error message
getId :: String -> Int -> String -> (String, String)
getId _ _ [] = ("ERROR: ID is empty", "ERROR: ID is empty")
getId id 0 rest = (id, rest)
getId id length rest = getId (id ++ [head rest]) (length-1) (tail rest)

readMark :: String -> Either String (Char, String)
readMark ('4':':':'p':'r':'e':'v':rest) = getMark (reverse rest)
readMark ('1':rest) = getMark (reverse ('1':rest))
readMark ('E':'R':'R':rest) = Left ('E':'R':'R':rest)
readMark msg = (Left "ERROR: Wrong format after ID")

getMark :: String -> Either String (Char, String)
getMark ('e':'x':':':'1':'v':':':'1':rest) = Right ('X', reverse rest)
getMark ('e':'X':':':'1':'v':':':'1':rest) = Right ('X', reverse rest)
getMark ('e':'o':':':'1':'v':':':'1':rest) = Right ('O', reverse rest)
getMark ('e':'O':':':'1':'v':':':'1':rest) = Right ('O', reverse rest)
getMark _ = Left "ERROR: Wrong mark format"

parseNumber :: Int -> Int -> String -> Int
parseNumber 0 1 [] = (-1) -- ERROR parsing number
parseNumber num i [] = num
parseNumber num i rest =
    let
        digit = last rest
    in
        
        case digit of '0' -> parseNumber (num + 0 * i) (i*10) (init rest)
                      '1' -> parseNumber (num + 1 * i) (i*10) (init rest)
                      '2' -> parseNumber (num + 2 * i) (i*10) (init rest)
                      '3' -> parseNumber (num + 3 * i) (i*10) (init rest)
                      '4' -> parseNumber (num + 4 * i) (i*10) (init rest)
                      '5' -> parseNumber (num + 5 * i) (i*10) (init rest)
                      '6' -> parseNumber (num + 6 * i) (i*10) (init rest)
                      '7' -> parseNumber (num + 7 * i) (i*10) (init rest)
                      '8' -> parseNumber (num + 8 * i) (i*10) (init rest)
                      '9' -> parseNumber (num + 9 * i) (i*10) (init rest)
                      _ -> (-1) -- ERROR parsing number

---------------------Moves -> String--------------------
getMessage :: [Move] -> String
getMessage moves = convertMoves moves []

convertMoves :: [Move] -> String -> String
convertMoves [] str = str
convertMoves moves str = 
        let 
            move = head moves
            coordsStr = "d1:cd1:0i" ++ show (x move) ++ "e1:1i" ++ show (y move) ++ "ee"
            idStr = "2:id" ++ show (length (pId move)) ++ ":" ++ pId move
            prevStr = if (str == []) then "" else "4:prev"
            markStr = "1:v1:" ++ [(mark move)] ++ "e"
        in
            convertMoves (tail moves) (coordsStr ++ idStr ++ prevStr ++ str ++ markStr) 


t :: String -> String
t str =
    let 
        moves = fromRight $ parseMoves [] str
    in
        getMessage moves

fromRight :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x
