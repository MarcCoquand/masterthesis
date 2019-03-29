module Coord where


import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Set.Extra      as Set
import           Test.QuickCheck     (Arbitrary)
import qualified Test.QuickCheck     as Quickcheck
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Gen as Quickcheck



-- * TYPES


data Direction
    = North
    | South
    deriving Show


type Coord = (Int,Int)


data Handle = MakeHandle
    { isCollision   :: Coord -> Bool
    -- | Coords that can not be attacked
    , isIndomitable :: Coord -> Bool
    }



-- * INSTANCES


instance Arbitrary Direction where
    arbitrary =
        do  rand <- Quickcheck.choose (True, False)
            if rand
                then return North
                else return South
    shrink North = [South]
    shrink South = []



-- * RULES


southRule :: Coord -> Set Coord
southRule (x,y) =
    Set.fromList [(x-1,y+1),(x-1,y-1),(x-2,y),(x-1,y)]


northRule :: Coord -> Set Coord
northRule (x,y) =
    Set.fromList [(x+1,y+1),(x+1,y-1),(x+2,y),(x+1,y)]


doubleMoveRule :: Bool -> Coord -> Set Coord
doubleMoveRule hasMoved (x,y) =
    if hasMoved then
        Set.fromList [(x+2,y),(x-2,y)]
    else
        Set.empty


directionRule :: Direction -> Coord -> Set Coord
directionRule direction position =
    case direction of
        North ->
            southRule position
        South ->
            northRule position



-- * PAWN


attackablePawnRule :: Handle -> Coord -> Set Coord
attackablePawnRule handle (x,y) =
    let
        attackRange =
            Set.fromList [(x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1,y-1)]
    in
         Set.filter (isIndomitable handle) attackRange


collisionPawnRule :: Handle -> Coord -> (Set Coord)
collisionPawnRule handle (x,y) =
    let
        set =
            Set.fromList [(x+1, y), (x+2, y), (x-1, y), (x-2,y)]
    in
        Set.filter (isCollision handle) set


-- | Set of illegal moves
pawnRuleSet :: Handle -> Bool -> Direction -> Coord -> (Set Coord)
pawnRuleSet handle hasMoved direction pos =
    Set.unions $
        [ doubleMoveRule hasMoved pos
        , directionRule direction pos
        , attackablePawnRule handle pos
        , collisionPawnRule handle pos
        ]


-- | Set of all moves, legal or illegal
pawnMoveSet :: Coord -> Set Coord
pawnMoveSet (x,y) =
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


pawnMoves :: Handle -> Bool -> Direction -> Coord -> Set Coord
pawnMoves handle hasMoved direction pos =
    let
        moveSet =
            pawnMoveSet pos

        invalidMoves =
            pawnRuleSet handle hasMoved direction pos

    in
        Set.difference moveSet invalidMoves



-- * KNIGHT


collisionKnightRule :: Handle -> Coord -> (Set Coord)
collisionKnightRule handle =
    Set.filter (isCollision handle) . knightMoveSet


-- | Set of all moves, legal or not
knightMoveSet :: Coord -> Set Coord
knightMoveSet (x,y) =
    Set.fromList
        [ (x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1)
        , (x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)
        ]


-- | Set of illegal moves
knightRuleSet :: Handle -> Coord -> (Set Coord)
knightRuleSet =
    collisionKnightRule


knightMoves :: Handle -> Coord -> (Set Coord)
knightMoves handle pos =
    let
        moveSet =
            knightMoveSet pos

        invalidMoves =
            knightRuleSet handle pos
    in
        Set.difference moveSet invalidMoves
