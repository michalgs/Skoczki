module Constants where

data Color =  NoColor | BlackColor | WhiteColor
    deriving (Read, Show, Enum, Eq, Ord)
data PieceType = Blank | Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Read, Show, Enum, Eq, Ord)
data Column = A | B | C | D | E | F | G | H
    deriving (Read, Show, Enum, Eq, Ord)
data Row = One | Two | Three | Four | Five | Six | Seven | Eight 
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


-- ♔♕♖♗♘♙

defaultPiece = Knight

appHeader = "♘ ♘ ♘ SKOCZKI bajo jajo THE GAME ♞ ♞ ♞"

mainMenuHeader = "Welcome!"
startGameOption = "1. Start"
exitGameOption = "2. Exit"

colorSelectionHeader = "Select the color of your pieces:"
whiteOption = "1. White"
blackOption = "2. Black"

playersColorIndicator = "You're playing as: "


emptyBoard :: [Field]
emptyBoard = [Field column row (Piece NoColor Blank) (setColor column row) | column <- allColumns, row <- allRows]

startingBoardWhite :: [Field]

startingBoardWhite = [Field column row (defaultPieceOrBlank row)  (setColor column row) | row <- allRowsReverse, column <- allColumns]

startingBoardBlack :: [Field]
startingBoardBlack = [Field column row (defaultPieceOrBlank row)  (setColor column row) | row <- allRows, column <- allColumns]


defaultPieceOrBlank row
    | row == One || row == Two = (Piece WhiteColor defaultPiece)
    | row == Seven || row == Eight = (Piece BlackColor defaultPiece)
    | otherwise = (Piece NoColor Blank)

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
allRows = [One .. Eight]
allRowsReverse = [Eight, Seven .. One]


getRowNumeric row = fromEnum row + 1