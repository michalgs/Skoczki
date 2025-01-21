module ConsoleGraphics where


import Prelude hiding (putStr, putStrLn)
import Data.ByteString.Char8 (putStr, putStrLn)
import Data.ByteString.UTF8 (fromString)
import System.Console.ANSI
import System.IO (stdout)
import Constants


-- Printing screens

printMainMenu = do
    putStrLn $ fromString appHeader
    putStrLn $ fromString mainMenuHeader
    putStrLn $ fromString startGameOption
    putStrLn $ fromString exitGameOption

printColorSelection = do
    putStrLn $ fromString appHeader
    putStrLn $ fromString colorSelectionHeader
    putStrLn $ fromString whiteOption
    putStrLn $ fromString blackOption


-- Printing board

printField playerColor field = do
    if ((playerColor == WhiteColor && column field == A) ||
        (playerColor == BlackColor && column field == H)) 
        then do 
            putStr $ fromString " "
            printRowName (row field)
    else putStr $ fromString ""
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid (getOppositeColor(color field))
             , SetColor Background Vivid (getColor(color field))
             ]
    putStr $ fromString (getPieceCode (piece field))
    setSGR [Reset]
    if ((playerColor == WhiteColor && column field == H) ||
        (playerColor == BlackColor && column field == A))
        then do
            printRowName (row field)
            putStr $ fromString "\n"
    else putStr $ fromString ""
    where backgroundColor = color

printGameState board playerColor = do
    printColumnNames playerColor
    printBoard board playerColor
    printColumnNames playerColor

printBoard :: [Field] -> Constants.Color -> IO()
printBoard board playerColor = mapM_ (printField playerColor) board


printColumnNames playerColor = do
    putStr $ fromString " "
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid White
             , SetColor Background Dull Yellow
             ]
    putStr $ fromString "   "
    if (playerColor == WhiteColor) then
        mapM_ printSingleBorderCell allColumns
    else mapM_ printSingleBorderCell allColumnsReverse
    putStr $ fromString "   "
    setSGR [Reset]
    putStr $ fromString "\n"

printSingleBorderCell symbol = do
    putStr $ fromString (show symbol ++ " ")

printRowName row = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid White
             , SetColor Background Dull Yellow
             ]
    putStr $ fromString (" " ++ show (getRowNumeric row) ++ " ")
    setSGR [Reset]

-- Helper functions

getColor color
    | color == BlackColor = Black
    | color == WhiteColor = White

getOppositeColor color
    | color == BlackColor = White
    | color == WhiteColor = Black

printInputIndicator = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid Cyan
             , SetColor Background Dull Black -- maybe vivid?
             ]
    putStr $ fromString ">>  "
    setSGR [Reset]

printColorIndicator color = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid Green
             , SetColor Background Dull Black
             ]
    putStr $ fromString playersColorIndicator
    putStr $ fromString (show colorValue)
    setSGR [Reset]
    putStr $ fromString "\n"
    where colorValue = getColor color

printWrongMoveIndicator = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid Red
             , SetColor Background Dull Black
             ]
    putStr $ fromString wrongMoveIndicator
    setSGR [Reset]
    putStr $ fromString "\n"

printPlayersWinIndicator = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid Blue
             , SetColor Background Dull Black
             ]
    putStr $ fromString playersWinIndicator
    putStr $ fromString "\n"
    setSGR [Reset]
    putStr $ fromString keyPressIndicator
    putStr $ fromString "\n"

printBotsWinIndicator = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid Red
             , SetColor Background Dull Black
             ]
    putStr $ fromString botsWinIndicator
    putStr $ fromString "\n"
    setSGR [Reset]
    putStr $ fromString keyPressIndicator
    putStr $ fromString "\n"