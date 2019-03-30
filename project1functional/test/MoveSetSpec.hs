module MoveSetSpec where

import           Control.Exception        (evaluate)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified MoveSet
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function (Fun)


pawnSetProperty ::
    Fun (Int,Int) Bool
    -> Fun (Int,Int) Bool
    -> (Int,Int)
    -> MoveSet.Direction
    -> Bool
    -> Gen Bool
pawnSetProperty (Fun _ isCollision) (Fun _ isAttackable) position direction hasMoved =
    let
        handler = MoveSet.MakeHandle isCollision isAttackable
        moves =
            MoveSet.pawn handler hasMoved direction position
    in
        return $ moves `Set.isProperSubsetOf` (MoveSet.pawnAll position)


knightSetProperty ::
    Fun (Int,Int) Bool
    -> Fun (Int,Int) Bool
    -> (Int,Int)
    -> Gen Bool
knightSetProperty (Fun _ isCollision) (Fun _ isAttackable) position =
    let
        handler = MoveSet.MakeHandle isCollision isAttackable
        moves =
            MoveSet.knight handler position
    in
        return $ moves `Set.isSubsetOf` (MoveSet.knightAll position)

spec :: Spec
spec =
    describe "Piece" $ do
        it "Pawn ruleset is proper subset" $
            property pawnSetProperty

        it "Knight ruleset is subset" $
            property knightSetProperty
