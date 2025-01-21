module Utils where

import Constants

split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

parseCharToRow digit = 
    if value > 8 || value < 1
        then error "Incorrect coordinate."
    else toEnum (value - 1)::Row
    where value = digitToInt digit

parseCharToColumn value = 
    if value < 'A' || value > 'H'
        then error "Incorrect coordinate"
    else toEnum (toEnum $ getIndexInAlphabet value)::Column


getIndexInAlphabet:: Char -> Int
getIndexInAlphabet letter = fromEnum (toLower letter) - fromEnum 'a'

getRowsBetween startRow endRow
      | startRow == Eight = []
      | nextRow >= endRow = []
      | otherwise = nextRow : getRowsBetween nextRow endRow
      where nextRow = succ startRow 
    
getColumnsBetween startColumn endColumn
      | startColumn == H = []
      | nextColumn >= endColumn = []
      | otherwise = nextColumn : getColumnsBetween nextColumn endColumn
      where nextColumn = succ startColumn

getFieldFromCoords board column row playerColor = board !! getIndex column row playerColor

getIndex column row playerColor
      | playerColor == WhiteColor = fromEnum column + (7 - fromEnum row) * 8
      | otherwise = (7 - fromEnum column) + fromEnum row * 8

checkBooleanList occupancyList
    | occupancyList == [] = True
    | head occupancyList == False = False
    | otherwise = checkBooleanList (tail occupancyList)


toLower letter =
      if letter >= 'A' && letter <= 'Z' then toEnum (fromEnum 'a' + letterIndex)
      else letter
      where letterIndex = fromEnum letter - fromEnum 'A'

digitToInt digit = fromEnum digit - fromEnum '0'

canMoveInDirection direction startingCoords distance
      | direction == UpD = (fromEnum row - distance) >= 0
      | direction == DownD = (row + distance) < 8
      | direction == LeftD = (column - distance) >= 0
      | direction == RightD = (column + distance) < 8
      where column = fromEnum (x startingCoords)
            row = fromEnum (y startingCoords)


getCoordsAfterMove direction startingCoords distance
      | direction == UpD = Coords column (toEnum(fromEnum row - distance)::Row)
      | direction == DownD = Coords column (toEnum(fromEnum row + distance)::Row)
      | direction == LeftD = Coords (toEnum(fromEnum column - distance)::Column) row
      | direction == RightD = Coords (toEnum(fromEnum column + distance)::Column) row
      where column = x startingCoords
            row = y startingCoords
