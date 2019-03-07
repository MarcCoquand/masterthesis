{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Game where


import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Control.Monad.State.Lazy (MonadState, StateT)
import qualified Control.Monad.State.Lazy as State
import Control.Monad (guard)
import qualified Board
import Board (Board)
import qualified Piece


data Player 
    = Black
    | White
    deriving (Eq)


data Piece 
    = Pawn 
    | Knight


data Square 
    = Blank
    | HasPiece Player Piece



instance (Monad m, MonadState (Player, Board Square) m) => Piece.HasCheck m where
    isCollision =
        do  (player, board) <- State.get
            return True
    isAttackable =
        return True


initialize :: Board Square
initialize = 
    undefined

        
