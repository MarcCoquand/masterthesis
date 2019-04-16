 module MoveSet where


import           Data.Function   ((&))
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as Quickcheck



data Direction
    = North
    | South
    deriving Show


type Coord = (Int,Int)


type MoveSet = Set Coord


-- | Handle contain functions that allow checking for collisions and
-- attackable squares on the board.
data Handle = MakeHandle
    { isCollision   :: Coord -> Bool
    -- | Coords that can not be attacked
    , isIndomitable :: Coord -> Bool
    }


instance Arbitrary Direction where
    arbitrary =
        do  rand <- Quickcheck.choose (True, False)
            if rand
                then return North
                else return South
    shrink North = [South]
    shrink South = []



-- * PAWN


southRule :: Coord -> MoveSet
southRule (x,y) =
    Set.fromList [(x-1,y+1),(x-1,y-1),(x-2,y),(x-1,y)]


northRule :: Coord -> MoveSet
northRule (x,y) =
    Set.fromList [(x+1,y+1),(x+1,y-1),(x+2,y),(x+1,y)]


doubleMoveRule :: Bool -> Coord -> MoveSet
doubleMoveRule hasMoved (x,y) =
    if hasMoved then
        Set.fromList [(x+2,y),(x-2,y)]
    else
        Set.empty


directionRule :: Direction -> Coord -> MoveSet
directionRule direction position =
    case direction of
        North ->
            southRule position
        South ->
            northRule position


attackablePawnRule :: Handle -> Coord -> MoveSet
attackablePawnRule handle (x,y) =
    let
        attackRange =
            Set.fromList [(x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1,y-1)]
    in
         Set.filter (isIndomitable handle) attackRange


collisionPawnRule :: Handle -> Coord -> MoveSet
collisionPawnRule handle (x,y) =
    let
        set =
            Set.fromList [(x+1, y), (x+2, y), (x-1, y), (x-2,y)]
    in
        Set.filter (isCollision handle) set


-- | Set of illegal moves
pawnRuleSet :: Handle -> Bool -> Direction -> Coord -> MoveSet
pawnRuleSet handle hasMoved direction pos =
    Set.unions
        [ doubleMoveRule hasMoved pos
        , directionRule direction pos
        , attackablePawnRule handle pos
        , collisionPawnRule handle pos
        ]


-- | Set of all moves, legal or illegal
pawnAll :: Coord -> MoveSet
pawnAll (x,y) =
    Set.fromList
        [ (x+1,y+1)
        , (x+1,y-1)
        , (x+2,y)
        , (x+1,y)
        , (x-1,y+1)
        , (x-1,y-1)
        , (x-2,y)
        , (x-1,y)
        ]


-- | The allowed moves for a pawn on the given coordinate
pawn :: Handle -> Bool -> Direction -> Coord -> MoveSet
pawn handle hasMoved direction pos =
    let
        moveSet =
            pawnAll pos

        invalidMoves =
            pawnRuleSet handle hasMoved direction pos

    in
        Set.difference moveSet invalidMoves



-- * KNIGHT


collisionKnightRule :: Handle -> Coord -> MoveSet
collisionKnightRule handle coord =
    knightAll coord
        & Set.filter (isCollision handle)


-- | Set of all moves, legal or not
knightAll :: Coord -> MoveSet
knightAll (x,y) =
    Set.fromList
        [ (x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1)
        , (x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)
        ]


-- | Set of illegal moves
knightRuleSet :: Handle -> Coord -> MoveSet
knightRuleSet =
    collisionKnightRule


-- | Allowed moves for a knight on the given coordinates
knight :: Handle -> Coord -> MoveSet
knight handle pos =
    let
        moveSet =
            knightAll pos

        invalidMoves =
            knightRuleSet handle pos
    in
        Set.difference moveSet invalidMoves
