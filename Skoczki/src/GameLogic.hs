module GameLogic (
    launchMainMenu,
    launchColorSelection,
    launchGameplay
) where

import System.Console.ANSI (clearScreen)
import System.Exit
import ConsoleGraphics
import Constants

launchMainMenu = do
    clearScreen
    printMainMenu
    selection <- getLine
    handleMenuSelection selection

launchColorSelection = do
    clearScreen
    printColorSelection
    selection <- getLine
    handleColorSelection selection

launchGameplay color = do
    clearScreen
    let board = selectStartingBoard color
    printGameState board


selectStartingBoard color
    | color == WhiteColor = startingBoardWhite
    | color == BlackColor = startingBoardBlack

exitGame = do
    putStr $ "Goodbye!"
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