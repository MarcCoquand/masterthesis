{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Piece where 


import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Extra as Set
import Control.Monad.State.Lazy (MonadState)
import qualified Control.Monad.State.Lazy as State


data Direction 
    = North
    | South


class Monad m => HasCheck m where
    isCollision :: Coord -> m Bool
    isAttackable :: Coord -> m Bool


instance HasCheck ((->) (Bool,Bool)) where 
    isCollision _ (b,_) = b
    isAttackable _ (_, b) = b


type Coord = (Int,Int)



-- RULES


attackablePawnRule :: HasCheck m => Coord -> m (Set Coord)
attackablePawnRule (x,y) =
    let 
        attackRange = 
            Set.fromList [(x+1, y+1), (x+1, y-1), (x-1, y-1), (x-1,y-1)]
    in 
        do  filtered <- Set.filterM isAttackable attackRange
            return filtered
            

collisionPawnRule :: HasCheck m => Coord -> m (Set Coord)
collisionPawnRule (x,y) =
    let 
        set = 
            Set.fromList [(x+1, y), (x+2, y), (x-1, y), (x-2,y)]
    in 
        do  filtered <- Set.filterM isCollision set
            return filtered
            

southRule :: Coord -> Set Coord
southRule (x,y) =
    Set.fromList [(x-1,y+1),(x-1,y-1),(x-2,y),(x-1,y)] 


northRule :: Coord -> Set Coord
northRule (x,y) =
    Set.fromList [(x+1,y+1),(x+1,y-1),(x+2,y),(x+1,y)]


doubleMoveRule :: Coord -> Set Coord
doubleMoveRule (x,y) = 
    Set.fromList [(x+2,y),(x-2,y)]



--  PAWN 


pawnRuleSet :: HasCheck m => Bool -> Direction -> Coord -> m (Set Coord)
pawnRuleSet hasMoved direction pos = 
    do  let move =
                if hasMoved then doubleMoveRule pos else Set.empty
        let dir =
                case direction of 
                    North ->
                        southRule pos
                    South -> 
                        northRule pos
        attack <- attackablePawnRule pos
        collision <- collisionPawnRule pos
        return . Set.unions $ [move,dir,attack,collision]


pawnMovesSet :: Coord -> Set Coord
pawnMovesSet (x,y) =
    Set.fromList 
        [(x+1,y+1)
        ,(x+1,y-1)
        ,(x+2,y)
        ,(x+1,y)
        ,(x-1,y+1)
        ,(x-1,y-1)
        ,(x-2,y)
        ,(x-1,y)
        ]


pawnMoves :: HasCheck m => Bool -> Direction -> Coord -> m (Set Coord)
pawnMoves hasMoved direction pos =
    do  let moveSet =  
                pawnMovesSet pos
        invalidMoves <- pawnRuleSet hasMoved direction pos
        return (Set.difference moveSet invalidMoves)



-- KNIGHT


collisionKnightRule :: HasCheck m => Coord -> m (Set Coord)
collisionKnightRule (x,y) =
    let 
        set = 
            Set.fromList 
                [(x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1)  
                ,(x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)  
                ]
    in 
        do  filtered <- Set.filterM isCollision set
            return filtered
            

knightRuleSet :: HasCheck m => Coord -> m (Set Coord)
knightRuleSet =
    collisionKnightRule 
    

knightMoveSet :: Coord -> Set Coord
knightMoveSet (x,y) =
    Set.fromList 
        [(x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1)  
        ,(x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)  
        ]  


knightMoves :: HasCheck m => Coord -> m (Set Coord)
knightMoves pos =
    do  let moveSet =
                knightMoveSet pos
        invalidMoves <- knightRuleSet pos
        return (Set.difference moveSet invalidMoves)
