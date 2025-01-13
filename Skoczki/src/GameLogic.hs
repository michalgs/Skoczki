module GameLogic where

import System.Console.ANSI (clearScreen)
import System.Exit
import ConsoleGraphics
import Constants

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
    let board = selectStartingBoard color
    runTheRound board color

runTheRound board color = do
    clearScreen
    printColorIndicator color
    printGameState board
    printInputIndicator
    move <- getLine
    let newBoard = handleMoveSelection move board
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


handleMoveSelection move board = board