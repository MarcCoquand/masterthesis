{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
module CoordSpec where

import           Control.Exception        (evaluate)
import qualified Coord
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function (Fun)


pawnSetProperty ::
    Fun (Int,Int) Bool
    -> Fun (Int,Int) Bool
    -> (Int,Int)
    -> Coord.Direction
    -> Bool
    -> Gen Bool
pawnSetProperty (Fun _ isCollision) (Fun _ isAttackable) position direction hasMoved =
    let
        handler = Coord.MakeHandle isCollision isAttackable
        moves =
            Coord.pawnMoves handler hasMoved direction position
    in
        return $ moves `Set.isProperSubsetOf` (Coord.pawnMoveSet position)


knightSetProperty ::
    Fun (Int,Int) Bool
    -> Fun (Int,Int) Bool
    -> (Int,Int)
    -> Gen Bool
knightSetProperty (Fun _ isCollision) (Fun _ isAttackable) position =
    let
        handler = Coord.MakeHandle isCollision isAttackable
        moves =
            Coord.knightMoves handler position
    in
        return $ moves `Set.isSubsetOf` (Coord.knightMoveSet position)

spec :: Spec
spec =
    describe "Piece" $ do
        it "Pawn ruleset is proper subset" $
            property pawnSetProperty

        it "Knight ruleset is subset" $
            property knightSetProperty
