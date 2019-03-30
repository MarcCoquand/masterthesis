module Piece where


data Piece
    = Pawn Bool -- ^ Has the pawn moved or not
    | Knight


instance Show Piece where
    show (Pawn _) = "P"
    show Knight   = "K"


update :: Piece -> Piece
update Knight =
    Knight
update (Pawn _) =
    -- Pawn has moved once
    Pawn True

