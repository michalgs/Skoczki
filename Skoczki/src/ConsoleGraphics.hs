module ConsoleGraphics (
    printMainMenu,
    printBoard
) where


import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)
import System.Console.ANSI
import System.IO (stdout)
import Constants

printMainMenu = do
    putStr $ fromString appHeader


printField field = do
    if (row field /= Eight && column field == A) 
        then putStr $ fromString "\n"
    else putStr $ fromString ""
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid (getOppositeColor(color field))
             , SetColor Background Vivid (getColor(color field))
             ]
    putStr $ fromString (getPieceCode (piece field))
    setSGR [Reset]
    where backgroundColor = color

getPieceCode piece
    | color == BlackColor = getBlackPiece _type
    | color == WhiteColor = getWhitePiece _type
    | color == NoColor = "  "
    where color = pieceColor piece
          _type = pieceType piece

printBoard :: [Field] -> IO()
printBoard board = do mapM_ printField board

getColor color
    | color == BlackColor = Black
    | color == WhiteColor = White

getOppositeColor color
    | color == BlackColor = White
    | color == WhiteColor = Black

-- ♔♕♖♗♘♙♚♛♜♝♞♟

printColumn 