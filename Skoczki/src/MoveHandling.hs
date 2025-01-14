module MoveHandling where


import Data.Char
import Constants
import Utils


getMovesList moves = [ parseMoveToCoords move  | move <- parseInput moves]

parseInput :: [Char] -> [[Char]]
parseInput = split (=='-')

parseMoveToCoords :: [Char] -> Coords
parseMoveToCoords move = 
    if length move == 2 then
        Coords (parseCharToColumn (head move)) (parseCharToRow (last move))
    else error "Incorrect move format."



parseCharToRow digit = 
    if value > 8 || value < 1
        then error "Incorrect coordinate."
    else toEnum (value - 1)::Row
    where value = digitToInt digit

parseCharToColumn value = toEnum (toEnum $ getIndexInAlphabet value)::Column

getIndexInAlphabet:: Char -> Int
getIndexInAlphabet letter = fromEnum (toLower letter) - fromEnum 'a'




makeMove startCoords finishCoords board color = do
    swapBoardField finishCoords (getDefaultPiece color) newBoard
    where newBoard = swapBoardField startCoords getBlankPiece board


changeFieldPiece field newPiece = field{piece = newPiece}

swapBoardField coords newPiece board = swapBoardFieldHelper coords newPiece [] (head board) (tail board) 


swapBoardFieldHelper :: Coords -> Piece -> [Field] -> Field -> [Field] -> [Field]
swapBoardFieldHelper coords newPiece prevFields currentField nextFields
    | matchingCoords boardX boardY destX destY = prevFields ++ [(changeFieldPiece currentField newPiece)] ++ nextFields
    | otherwise = swapBoardFieldHelper coords newPiece (prevFields ++ [currentField]) (head nextFields) (tail nextFields)
    where boardX = (column currentField)
          boardY = (row currentField)
          destX = (x coords)
          destY = (y coords) 

matchingCoords x1 y1 x2 y2 = (x1 == x2) && (y1 == y2)