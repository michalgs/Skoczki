module MoveHandling where


import Constants
import Utils


getMovesList moves = [ parseMoveToCoords move  | move <- parseInput moves]

parseInput :: [Char] -> [[Char]]
parseInput = split (=='-')

parseMoveToCoords :: [Char] -> Coords
parseMoveToCoords move = 
    if length move == 2 then
        Coords (parseCharToColumn (head move)) (parseCharToRow (last move))
    else error "Incorrect coordinate."


-- Placing piece on correct coords

makeMove startCoords finishCoords board color =
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

-- Parsing the list of coords into the list with tuples representing moves and then checking legality of every jump

moveIsLegal :: [Coords] -> [Field] -> Color -> Color -> Bool
moveIsLegal movesList currentBoard currentColor playerColor =
    startPieceIsLegal startField currentColor && everyJumpIsLegal (checkEveryJump movesList currentBoard playerColor)
    where startCoords = head movesList
          startField = getFieldFromCoords currentBoard (x startCoords) (y startCoords) playerColor

everyJumpIsLegal [] = True
everyJumpIsLegal legalityList = checkBooleanList legalityList

checkEveryJump :: [Coords] -> [Field] -> Color -> [Bool]
checkEveryJump movesList currentBoard currentColor = 
    map (jumpIsLegal currentBoard currentColor) (generateMovePairs (init movesList) (tail movesList))

generateMovePairs :: [Coords] -> [Coords] -> [[Coords]]
generateMovePairs [] [] = []
generateMovePairs startCoords finishCoords = 
    [head startCoords, head finishCoords] : generateMovePairs (tail startCoords) (tail finishCoords)

-- Core legality-checking logic

jumpIsLegal :: [Field] -> Color -> [Coords] -> Bool
jumpIsLegal currentBoard currentColor [startCoords, finishCoords] 
    | (x startCoords == x finishCoords) && (y startCoords /= y finishCoords)
        = checkRowMove currentBoard currentColor startCoords finishCoords
    | (y startCoords == y finishCoords) && (x startCoords /= x finishCoords) 
        = checkColumnMove currentBoard currentColor startCoords finishCoords
    | otherwise = False

rowOccupancyList currentBoard column rows playerColor = 
    [piece (getFieldFromCoords currentBoard column row playerColor) /= getBlankPiece | row <- rows]

checkRowMove currentBoard currentColor startCoords finishCoords =
    finishPieceIsBlank (getFieldFromCoords currentBoard (x finishCoords) (y finishCoords) currentColor) 
        && checkBooleanList occupancyList
    where column = x startCoords
          rows = getRowsBetween (y startCoords) (y finishCoords)
          occupancyList = rowOccupancyList currentBoard column rows currentColor

columnOccupancyList currentBoard row columns playerColor = 
    [piece (getFieldFromCoords currentBoard column row playerColor) /= getBlankPiece | column <- columns]

checkColumnMove currentBoard currentColor startCoords finishCoords =
    finishPieceIsBlank (getFieldFromCoords currentBoard (x finishCoords) (y finishCoords) currentColor) 
        && checkBooleanList occupancyList
    where row = y startCoords
          columns = getColumnsBetween (x startCoords) (x finishCoords)
          occupancyList = columnOccupancyList currentBoard row columns currentColor

startPieceIsLegal field currentColor = (piece field) == (getDefaultPiece currentColor)

finishPieceIsBlank field = (piece field) == getBlankPiece