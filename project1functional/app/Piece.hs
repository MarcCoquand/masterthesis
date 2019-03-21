{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
module Piece where


import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Set.Extra      as Set
import qualified Test.QuickCheck     as Quickcheck
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Gen as Quickcheck



-- * TYPES


data Direction
    = North
    | South


type Coord = (Int,Int)


class Monad m => HasCheck m where
    isCollision :: Coord -> m Bool
    -- | Coords that can not be attacked
    isIndomitable :: Coord -> m Bool



-- * INSTANCES


-- | Enable dependency injection as a last argument, For instance calling
-- > pawnMoves True South (0,0) (True,True)
-- would evaluate isCollision = True and isIndomitable = True
instance HasCheck ((->) (Bool,Bool)) where
    isCollision _ (collision,_) = collision
    isIndomitable _ (_, indomitable) = indomitable


-- | Allow to be tested with Quickcheck
instance HasCheck Gen where
    isCollision _ =
         Quickcheck.arbitrary
    isIndomitable _ =
         Quickcheck.arbitrary



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


attackablePawnRule :: HasCheck m => Coord -> m (Set Coord)
attackablePawnRule (x,y) =
    let
        attackRange =
            Set.fromList [(x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1,y-1)]
    in
         Set.filterM isIndomitable attackRange


collisionPawnRule :: HasCheck m => Coord -> m (Set Coord)
collisionPawnRule (x,y) =
    let
        set =
            Set.fromList [(x+1, y), (x+2, y), (x-1, y), (x-2,y)]
    in
        Set.filterM isCollision set


-- | Set of illegal moves
pawnRuleSet :: HasCheck m => Bool -> Direction -> Coord -> m (Set Coord)
pawnRuleSet hasMoved direction pos =
    do  attackRule <- attackablePawnRule pos
        collisionRule <- collisionPawnRule pos
        return . Set.unions $
            [ doubleMoveRule hasMoved pos
            , directionRule direction pos
            , attackRule
            , collisionRule
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


pawnMoves :: HasCheck m => Bool -> Direction -> Coord -> m (Set Coord)
pawnMoves hasMoved direction pos =
    let
        moveSet =
            pawnMoveSet pos

    in  do  invalidMoves <- pawnRuleSet hasMoved direction pos
            return $ Set.difference moveSet invalidMoves



-- * KNIGHT


collisionKnightRule :: HasCheck m => Coord -> m (Set Coord)
collisionKnightRule =
    Set.filterM isCollision . knightMoveSet


-- | Set of all moves, legal or not
knightMoveSet :: Coord -> Set Coord
knightMoveSet (x,y) =
    Set.fromList
        [ (x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1)
        , (x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)
        ]


-- | Set of illegal moves
knightRuleSet :: HasCheck m => Coord -> m (Set Coord)
knightRuleSet =
    collisionKnightRule


knightMoves :: HasCheck m => Coord -> m (Set Coord)
knightMoves pos =
    do  let moveSet =
                knightMoveSet pos
        invalidMoves <- knightRuleSet pos
        return $ Set.difference moveSet invalidMoves
