module Piece where


type HasMoved =
    Bool


data Piece
    = Pawn HasMoved
    | Knight
    deriving (Eq)


instance Show Piece where
    show (Pawn _) = "P"
    show Knight   = "K"


update :: Piece -> Piece
update piece =
    case piece of
        Knight ->
            Knight

        Pawn _ ->
            Pawn True

