import Data.List

type Move = (Int, Int, Char)
type Moves = [Move]

message :: String
message = "d1:cd1:0i0e1:1i2ee2:id3:vWk4:prevd1:cd1:0i1e1:1i1ee2:id3:vWk1:v1:oe1:v1:xe"
--(0, 2) = X by "vWk"
--(1, 1) = O by "vWk"

-- message = "d1:cd1:0i2e1:1i1ee2:id4:pAtK1:v1:xe"
-- (2, 1) = X by "pAtK"


validate :: String -> String --Bool
validate str =  beginParse str

beginParse :: String -> String
beginParse ('d': rest) = "yeah"
beginParse _ = error "bye bye"
