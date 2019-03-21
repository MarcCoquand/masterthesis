module Board
    ( Coord
    , Board
    , update
    , boardSize
    , upperBound
    , makeCoord
    , get
    , coordSet
    , construct
    , extractCoord
    , maybeGet
    ) where


import           Data.Array     (Array, (!), (//))
import qualified Data.Array     as Array
import           Data.List      (foldl', unfoldr)
import           Data.Maybe     (catMaybes)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import qualified Data.Set.Extra as Set


-- | By hiding the constructor all coords must be created through the makeCoord
-- and catCoords functions which check that the Coordinate is a valid one. Thus
-- no unsafe coordinates can be introduced.
newtype Coord = UnCoord (Int,Int)
    deriving (Ord, Eq, Show, Array.Ix)


newtype Board element = Board (Array Coord element)
    deriving (Eq, Ord)


addNumberToString :: Int -> String -> String
addNumberToString n string =
    show n ++ ". " ++ string


toString :: Show e => Board e -> [String]
toString (Board arr) =
    [unwords
        [show (arr ! UnCoord (y, x)) | x <- [1..boardSize]]
        | y <- [1..boardSize]
            ]


makeNumberList :: Int -> Int -> String
makeNumberList amount =
    unwords . take amount . unfoldr (\i -> Just (show i ++ ".", i+1))


withIndex :: [String] -> [String]
withIndex =
    ["   " ++ makeNumberList (length boardString) 1] <>
    -- Prefer foldl' over foldl for efficiency
    foldl'
        (\board next ->
            board <> [addNumberToString (length board + 1) next])
        []


instance Show e => Show (Board e) where
    show =
        unlines . withIndex . toString


-- | For safety reasons and since a chess game requires only one board we
-- introduce a variable for the board size that is fixed. This simplifies the
-- API to make it impossible to create a coordinate that is outside of index.
boardSize :: Int
boardSize = 8


upperBound :: Board e -> (Int, Int)
upperBound (Board arr) =
    let
        (lower, UnCoord upper) =
            Array.bounds arr
    in
        upper


extractCoord :: Coord -> (Int,Int)
extractCoord (UnCoord pos) =
    pos


makeCoord :: Board e -> (Int, Int) -> Maybe Coord
makeCoord board coord =
    if coord `isWithinRange` upperBound board then
        return . UnCoord $ coord
    else
        Nothing


isWithinRange :: (Int, Int) -> (Int,Int) -> Bool
isWithinRange (c1,c2) (c3,c4) =
    c1 <= c3 && c2 <= c4 && c1 > 0 && c2 > 0


get :: Board square -> Coord -> square
get pos@(Board arr) =
    getFromArray arr
    where
        -- getFromArray is unsafe but since Coords can only be created if
        -- they're in the bounds of the array we have made it safe and the array
        -- is unexposed the operation becomes safe.
        getFromArray = (!)


maybeGet :: Board square -> (Int,Int) -> Maybe square
maybeGet board =
    fmap (get board) . makeCoord board


update :: Board square -> [(Coord, square)] -> Board square
update (Board arr) =
    Board . updateArray arr
    where
        -- When libraries introduce unclear operators I replace them with a
        -- documenting name
        updateArray = (//)


-- | Eliminates all out of bounds coordinates
coordSet ::  Board e -> Set (Int,Int) -> Set Coord
coordSet board =
    Set.catMaybes . Set.map (makeCoord board)


-- | Takes a list of squares that needs to have a
-- (boardSize) * (boardSize) amount of elements.
construct :: [square] -> Maybe (Board square)
construct squares =
    let
        squareAmount =
            length squares
        availableSlots =
            boardSize * boardSize
    in
        if squareAmount == availableSlots then
            return
                . Board
                $ Array.listArray
                    (UnCoord (1,1), UnCoord (boardSize, boardSize)) squares
        else
            Nothing
