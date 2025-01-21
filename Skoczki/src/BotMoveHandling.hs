module BotMoveHandling where

import System.Random
import MoveHandling
import Constants
import Utils



findStartingField:: [Field] -> Color -> IO Coords
findStartingField board playerColor = do
    randCol <- getRandomColumn
    randRow <- getRandomRow
    let possibleStartingField = getFieldFromCoords board randCol randRow playerColor
    let possibleStartingCoords = Coords randCol randRow 
    let botColor = getBotColor playerColor
    let botPiece = Piece botColor Knight
    if (piece possibleStartingField == botPiece)
        then return (possibleStartingCoords)
    else findStartingField board playerColor
          
          
findLegalFinishingCoords board startingCoords playerColor =
    checkAround board startingCoords playerColor 1

checkAround board startingCoords playerColor radius
    | legalDirection board UpD startingCoords radius playerColor = getCoordsAfterMove UpD startingCoords radius
    | legalDirection board LeftD startingCoords radius playerColor = getCoordsAfterMove LeftD startingCoords radius
    | legalDirection board DownD startingCoords radius playerColor = getCoordsAfterMove DownD startingCoords radius
    | legalDirection board RightD startingCoords radius playerColor = getCoordsAfterMove RightD startingCoords radius
    | otherwise = checkAround board startingCoords playerColor (radius + 1)
    

legalDirection board direction startingCoords distance playerColor =
    if (canMoveInDirection direction startingCoords distance) then do
        let finishCoords = getCoordsAfterMove direction startingCoords distance
        let moves = [startingCoords,finishCoords]
        moveIsLegal moves board (getBotColor playerColor) playerColor
    else False

getRandomColumn :: IO Column
getRandomColumn = do
    index <- getRandomIndexNumber 
    return (toEnum index)          

getRandomRow :: IO Row
getRandomRow = do
    index <- getRandomIndexNumber  
    return (toEnum index)         

getRandomIndexNumber :: IO Int
getRandomIndexNumber = randomRIO (0, 7)


getBotColor playerColor
    | playerColor == WhiteColor = BlackColor
    | otherwise = WhiteColor

