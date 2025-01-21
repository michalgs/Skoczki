module GameLogic where

import System.Console.ANSI (clearScreen)
import System.Exit
import ConsoleGraphics
import Constants
import MoveHandling
import Utils

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
    runTheRound (selectStartingBoard color) color

runTheRound board color = do
    printColorIndicator color
    printGameState board color
    printInputIndicator
    moves <- getLine
    let legalMove = (moveIsLegal (getMovesList moves) board color color)
    clearScreen
    let newBoard = changeBoardOnCondition legalMove board (handleMoveSelection moves board color)
    runTheRound newBoard color

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
