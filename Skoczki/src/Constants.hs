module Constants where

data Color =  NoColor | BlackColor | WhiteColor
    deriving (Read, Show, Enum, Eq, Ord)
data PieceType = Blank | Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Read, Show, Enum, Eq, Ord)
data Column = A | B | C | D | E | F | G | H
    deriving (Read, Show, Enum, Eq, Ord)
data Row = One | Two | Three | Four | Five | Six | Seven | Eight 
    deriving (Read, Show, Enum, Eq, Ord)
data Direction = UpD | DownD | LeftD | RightD
    deriving (Read, Show, Enum, Eq, Ord)
data Winner = None | Player | Bot
    deriving (Read, Show, Enum, Eq, Ord)

data Field = Field {
    column :: Column,
    row :: Row,
    piece :: Piece,
    color :: Color
} deriving (Read, Show, Eq, Ord)

data Piece = Piece {
    pieceColor :: Color,
    pieceType :: PieceType
} deriving (Read, Show, Eq, Ord)

data Coords = Coords {
    x :: Column,
    y :: Row
} deriving (Read, Show, Eq, Ord)

-- ♔♕♖♗♘♙

defaultPieceType = Knight

appHeader = "♘ ♘ ♘ SKOCZKI THE GAME ♞ ♞ ♞"

mainMenuHeader = "Welcome!"
startGameOption = "1. Start"
exitGameOption = "2. Exit"

colorSelectionHeader = "Select the color of your pieces:"
whiteOption = "1. White"
blackOption = "2. Black"

playersColorIndicator = "You're playing as: "
wrongMoveIndicator = "Illegal move, try again..."
playersWinIndicator = "Congratulations, you've won!"
botsWinIndicator = "Unfortunately computer has won... Better luck next time!"
keyPressIndicator = "Press Enter to continue..."

emptyBoard :: [Field]
emptyBoard = [Field column row (Piece NoColor Blank) (setColor column row) 
                | column <- allColumns, row <- allRows]

startingBoardWhite :: [Field]

startingBoardWhite = [Field column row (defaultPieceOrBlank row)  (setColor column row) 
                        | row <- allRowsReverse, column <- allColumns]

startingBoardBlack :: [Field]
startingBoardBlack = [Field column row (defaultPieceOrBlank row)  (setColor column row) 
                        | row <- allRows, column <- allColumnsReverse]



defaultPieceOrBlank row
    | row == One || row == Two = (Piece WhiteColor defaultPieceType)
    | row == Seven || row == Eight = (Piece BlackColor defaultPieceType)
    | otherwise = (Piece NoColor Blank) -- getBlankPiece

setColor xCoord yCoord
    | even diff = WhiteColor
    | otherwise = BlackColor
    where x = fromEnum xCoord
          y = fromEnum yCoord
          diff = abs (x - y)

getPieceCode piece
    | color == BlackColor = getBlackPiece _type
    | color == WhiteColor = getWhitePiece _type
    | color == NoColor = "  "
    where color = pieceColor piece
          _type = pieceType piece

getDefaultPiece color = Piece color defaultPieceType
getBlankPiece = Piece NoColor Blank

getWhitePiece :: PieceType -> String
getWhitePiece piece
    | piece == Pawn = "♟ "
    | piece == Knight = "♞ "
    | piece == Bishop = "♝ "
    | piece == Rook = "♜ "
    | piece == Queen = "♛ "
    | piece == King = "♚ "

getBlackPiece :: PieceType -> String
getBlackPiece piece
    | piece == Pawn = "♙ "
    | piece == Knight = "♘ "
    | piece == Bishop = "♗ "
    | piece == Rook = "♖ "
    | piece == Queen = "♕ "
    | piece == King = "♔ "

allColumns = [A .. H]
allColumnsReverse = [H, G .. A]
allRows = [One .. Eight]
allRowsReverse = [Eight, Seven .. One]


getRowNumeric row = fromEnum row + 1

