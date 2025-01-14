module MoveHandling where


import Data.Char
import Constants
import Utils



parseInput :: [Char] -> [[Char]]
parseInput = split (=='-')

parseMoveToCoords :: [Char] -> Coords
parseMoveToCoords move = 
    if length move == 2 then
        Coords (parseCharToColumn (head move)) (parseCharToRow (last move))
    else error "Incorrect move format."

getMovesArray moves = [ parseMoveToCoords move  | move <- parseInput moves]

parseCharToRow digit = 
    if value > 8 || value < 1
        then error "Incorrect coordinate."
    else toEnum (value - 1)::Row
    where value = digitToInt digit

parseCharToColumn value = toEnum (toEnum $ getIndexInAlphabet value)::Column

getIndexInAlphabet:: Char -> Int
getIndexInAlphabet letter = fromEnum (toLower letter) - fromEnum 'a'




makeMove startCoords finishCoords board index = 
    | 



changeFieldPiece field newPiece = field{piece = newPiece}
