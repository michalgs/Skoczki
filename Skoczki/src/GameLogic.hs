module GameLogic where

import System.Console.ANSI (clearScreen)
import System.Exit
import ConsoleGraphics
import Constants
import MoveHandling
import Utils
import BotMoveHandling

launchMainMenu = do
    clearScreen
    printMainMenu
    printInputIndicator
    selection <- getLine
    handleMenuSelection selection

launchColorSelection = do
    clearScreen
    printColorSelection
    printInputIndicator
    selection <- getLine
    handleColorSelection selection

launchGameplay color = do
    clearScreen
    runTheRound (selectStartingBoard color) color None

runTheRound board color winner = do
    if (winner == Player) then do
        printPlayersWinIndicator
        printGameState board color
        temp <- getLine
        launchMainMenu
    else if (winner == Bot) then do
        printBotsWinIndicator
        printGameState board color
        temp <- getLine
        launchMainMenu
    else do
        printColorIndicator color
        printGameState board color
        printInputIndicator
        moves <- getLine
        let legalMove = (moveIsLegal (getMovesList moves) board color color)
        clearScreen
        let newBoard = changeBoardOnCondition legalMove board (handleMoveSelection moves board color)
        finalBoard <- if legalMove then do
            botStartCoords <- findStartingField newBoard color
            let botFinishCoords = findLegalFinishingCoords board botStartCoords color
            let botMoveBoard = makeMove botStartCoords botFinishCoords newBoard (getBotColor color)
            return botMoveBoard
        else do 
            printWrongMoveIndicator
            return newBoard
        runTheRound finalBoard color (isGameFinished newBoard color)

selectStartingBoard color
    | color == WhiteColor = startingBoardWhite
    | color == BlackColor = startingBoardBlack

exitGame = do
    clearScreen
    putStrLn $ "Goodbye!"
    exitWith ExitSuccess

selectColor = do
    printColorSelection
    selection <- getLine
    handleColorSelection selection

handleMenuSelection selection
    | selection == "1" = launchColorSelection
    | selection == "2" = exitGame
    | otherwise = launchMainMenu

handleColorSelection selection
    | selection == "1" = launchGameplay WhiteColor
    | selection == "2" = launchGameplay BlackColor
    | otherwise = launchColorSelection


-- Move handling

handleMoveSelection moves board color = 
    makeMove (head movesList) (last movesList) board color
    where movesList = getMovesList moves 

changeBoardOnCondition condition oldBoard newBoard =
    if condition then newBoard
    else oldBoard

isGameFinished board playerColor
    | checkBooleanList ([(piece (getFieldFromCoords board row column playerColor)) == getDefaultPiece playerColor 
                            | row <- (getOpponentRows playerColor), column <- allColumns]) = Player
    | checkBooleanList ([(piece (getFieldFromCoords board row column playerColor)) == getDefaultPiece botColor 
                            | row <- (getOpponentRows botColor), column <- allColumns]) = Bot
    | otherwise = None
    where botColor = getBotColor playerColor



getOpponentRows playerColor
    | playerColor == WhiteColor = [Seven, Eight]
    | otherwise = [One, Two]