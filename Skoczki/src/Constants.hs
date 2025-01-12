module Constants (
    appHeader,
    getBlackPiece,
    getWhitePiece,
    emptyBoard,
    startingBoard,
    Color(..),
    Field(..),
    Piece(..),
    PieceType(..),
    Row(..),
    Column(..),
) where

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

defaultPiece = Knight

appHeader = "______SKOCZKI THE GAME______"

startGameOption = "1. Start"
exitGameOption = "2. Exit"

emptyBoard :: [Field]
emptyBoard = [Field column row (Piece NoColor Blank) (getColor column row) | column <- [A .. H], row <- [One .. Eight]]

startingBoard = [Field column row (defaultPieceOrBlank row)  (getColor column row) | row <- [Eight, Seven .. One], column <- [A .. H]]


defaultPieceOrBlank row
    | row == One || row == Two = (Piece WhiteColor defaultPiece)
    | row == Seven || row == Eight = (Piece BlackColor defaultPiece)
    | otherwise = (Piece NoColor Blank)

getColor xCoord yCoord
    | even diff = WhiteColor
    | otherwise = BlackColor
    where x = fromEnum xCoord
          y = fromEnum yCoord
          diff = abs (x - y)

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
