module Player where


data Player
    = Black
    | White
    deriving (Eq, Show)


next :: Player -> Player
next Black = White
next White = Black
