module Player where


data Player
    = Black
    | White
    deriving (Eq)


next :: Player -> Player
next Black = White
next White = Black


instance Show Player where
    show Black = "Black"
    show White = "White"

