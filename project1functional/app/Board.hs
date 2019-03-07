module Board 
    ( Coord
    , Board
    , boardSize
    , upperBound
    , makeCoord
    , get
    , coordSet
    , construct
    , knightMoves
    , extractCoord
    ) where


import qualified Data.Array as Array 
import Data.Array (Array, (!), (//))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Extra as Set


-- | By hiding the constructor all coords must be created through the makeCoord
-- and catCoords functions which check that the Coordinate is a valid one. Thus
-- no unsafe coordinates can be introduced.
newtype Coord = UnCoord (Int,Int)
    deriving (Ord, Eq, Show, Array.Ix)


newtype Board e = Board (Array Coord e)
    deriving (Show, Eq, Ord)


-- | For safety reasons and since the requirements are only one board we 
-- introduce a variable for the board size that is fixed. This way it is
-- impossible to create a coordinate that is outside of index.
boardSize :: Int
boardSize = 8


upperBound :: Board e -> (Int, Int)
upperBound (Board arr) =
    let 
        (lower, (UnCoord upper)) =
            Array.bounds arr
    in 
        upper


extractCoord :: Coord -> (Int,Int)
extractCoord (UnCoord pos) =
    pos


makeCoord :: Board e -> (Int, Int) -> Maybe Coord
makeCoord board coord =
    if (coord `isWithinRange` (upperBound board)) then
        return (UnCoord coord)
    else 
        Nothing


isWithinRange :: (Int, Int) -> (Int,Int) -> Bool
isWithinRange (c1,c2) (c3,c4) =
    if (c1 <= c3 && c2 <= c4 && c1 > 0 && c2 > 0) then
        True
    else 
        False


get :: Board square -> Coord -> square
get pos@(Board arr) coord =
    arr ! coord


update :: Board square -> [(Coord, square)] -> Board square 
update (Board arr) newVals = 
    Board (arr // newVals)
    

-- | Eliminates all out of bounds coordinates 
coordSet ::  Board e -> Set (Int,Int) -> Set Coord
coordSet board =
    Set.catMaybes . Set.map (makeCoord board) 
    


-- COORD HELPERS

knightMoves :: (Int,Int) -> [(Int,Int)]
knightMoves (x,y) = 
        [(x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1)  
        ,(x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)  
        ]  


-- | Takes a list of squares that needs to have a 
-- (boardSize) * (boardSize) amount of elements.
construct :: [square] -> Maybe (Board square) 
construct squares =
    let 
        squareAmount = 
            length squares
        availableSlots = 
            (boardSize) * (boardSize)
    in 
        if squareAmount == availableSlots then
            return 
                . Board 
                $ Array.listArray 
                    (UnCoord (1,1), UnCoord (boardSize, boardSize)) squares
        else
            Nothing
