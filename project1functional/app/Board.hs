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
    , isLegalMove
    ) where


import           Data.Array      (Array, (!), (//))
import qualified Data.Array      as Array
import           Data.Function   ((&))
import           Data.List       (foldl', unfoldr)
import           Data.Maybe      (fromJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Data.Set.Extra  as Set
import           Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as Quickcheck


-- | By hiding the constructor all coords must be created through makeCoord
-- and catCoords. Both functions check that the Coordinate is a valid one. Thus
-- no out of bounds coordinates can be created.
newtype Coord = UnCoord (Int,Int)
    deriving (Ord, Eq, Show, Array.Ix)


newtype Board element = CreateBoard (Array Coord element)
    deriving (Eq, Ord)


-- For use with Quickcheck, generate arbitrary board.
instance Arbitrary e => Arbitrary (Board e) where
    arbitrary =
        do  pieces <- Quickcheck.vector (boardSize * boardSize)
            pieces
                & construct
                & fromJust
                & return


toString :: Show e => Board e -> [String]
toString (CreateBoard arr) =
    -- unwords turns ["bl","bl","bl"] into "bl bl bl"
    [unwords
        -- Show one row
        [show (arr ! UnCoord (y, x))  | x <- [1..boardSize]]
        -- Perform for all rows
        | y <- [1..boardSize]
        ]


isLegalMove
    :: Set Board.Coord
    -> Board.Coord
    -> Bool
isLegalMove moveSet destination =
    Set.member destination moveSet


makeNumberList :: Int -> Int -> String
makeNumberList amount startNumber =
    let
        format number =
            show number ++ "."

        formattedNumberGenerator =
            unfoldr (\i -> Just (format i, i+1)) startNumber
    in
        formattedNumberGenerator
            & take amount
            & unwords -- Turns ["1.","2.","3."] into "1. 2. 3."


appendNumber :: Int -> String -> String
appendNumber n string =
    show n ++ ". " ++ string


makeIndex :: [String] -> [String]
makeIndex boardString =
    let
        topRow =
            "   " ++ makeNumberList (length boardString) 1

        boardRows =
            -- Prefer foldl' over foldl for efficiency and avoiding space leaks.
            foldl' appendRow [] boardString

        appendRow board row =
            board ++ [appendNumber (length board + 1) row]

    in
        [topRow] ++ boardRows


instance Show e => Show (Board e) where
    show board =
        board
            & toString
            & makeIndex
            & unlines


-- | For safety reasons and since a chess game requires only one board we
-- introduce a variable for the board size that is fixed. This simplifies the
-- API to make it impossible to create a coordinate that is outside of index.
boardSize :: Int
boardSize = 8


upperBound :: Board e -> (Int, Int)
upperBound (CreateBoard arr) =
    let
        (_, UnCoord upper) =
            Array.bounds arr
    in
        upper


extractCoord :: Coord -> (Int,Int)
extractCoord (UnCoord pos) =
    pos


makeCoord :: Board e -> (Int, Int) -> Maybe Coord
makeCoord board coord =
    if coord `isWithinRange` upperBound board then
        coord
            & UnCoord
            & return
    else
        Nothing


isWithinRange :: (Int, Int) -> (Int,Int) -> Bool
isWithinRange (c1,c2) (c3,c4) =
    c1 <= c3 && c2 <= c4 && c1 > 0 && c2 > 0


get :: Board square -> Coord -> square
get (CreateBoard arr) coord =
    -- (!) will crash if given out of bounds coordinates.
    -- However since Coords can only be created if they're in the bounds this
    -- function becomes safe.
    arr ! coord


maybeGet :: Board square -> (Int,Int) -> Maybe square
maybeGet board maybeCoord =
    maybeCoord
        & makeCoord board
        & fmap (get board)


update :: Board square -> [(Coord, square)] -> Board square
update (CreateBoard arr) changeList =
    changeList
        & updateArray arr
        & CreateBoard
    where
        updateArray = (//)


-- | Given a set of coordinates, return a set of coordinates that are in bounds
-- of the board.
coordSet ::  Board e -> Set (Int,Int) -> Set Coord
coordSet board set =
    set
        & Set.map (makeCoord board)
        & Set.catMaybes


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
           Array.listArray
                    (UnCoord (1,1), UnCoord (boardSize, boardSize)) squares
                & CreateBoard
                & return
        else
            Nothing
