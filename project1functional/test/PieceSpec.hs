{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
module PieceSpec where

import           Control.Exception        (evaluate)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Piece
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function (Fun)


pawnSetProperty :: Piece.HasCheck Gen
    => (Int,Int)
    -> Piece.Direction
    -> Bool
    -> Gen Bool
pawnSetProperty position direction hasMoved =
    do  moves <- Piece.pawnMoves hasMoved direction position
        return $ moves `Set.isProperSubsetOf` (Piece.pawnMoveSet position)


knightSetProperty :: Piece.HasCheck Gen
    => (Int,Int)
    -> Gen Bool
knightSetProperty position =
    do  moves <- Piece.knightMoves position
        return $ moves `Set.isProperSubsetOf` (Piece.knightMoveSet position)

spec :: Spec
spec =
    describe "Piece" $ do
        it "Pawn ruleset is proper subset" $
            verboseCheck pawnSetProperty

        it "Knight ruleset is subset" $
            quickCheck knightSetProperty
