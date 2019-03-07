{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Game where


import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Control.Monad.Writer.Lazy (MonadWriter)
import qualified Control.Monad.Writer.Lazy as Writer
import Control.Monad.State.Lazy (MonadState)
import qualified Control.Monad.State.Lazy as State
import Control.Monad (guard)
import qualified Board
import Board (Board)
import qualified Piece
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad ((<=<))



-- TYPES


data Player 
    = Black
    | White
    deriving (Eq)


data Piece 
    = Pawn Bool -- Has moved?
    | Knight


data Square 
    = Blank
    | HasPiece Player Piece


type HasGame m = (MonadState (Player, Board Square) m, Piece.HasCheck m)



-- INSTANCES


collisionCheck :: Square -> Bool
collisionCheck Blank =
    False
collisionCheck (HasPiece _ _ ) =
    True


attackCheck :: Player -> Square -> Bool
attackCheck player Blank =
    False
attackCheck player (HasPiece pieceOwner _) =
    player /= pieceOwner


instance (Monad m, MonadState (Player, Board Square) m) => Piece.HasCheck m where
    isCollision coord =
        do  (player, board) <- State.get
            let maybeCoord =
                    Board.makeCoord board coord
            case maybeCoord of 
                Just c ->
                    let 
                        square =
                            Board.get board c
                    in 
                        return . collisionCheck $ square
                Nothing ->
                    return False

    isAttackable coord =
        do  (player, board) <- State.get
            let maybeCoord =
                    Board.makeCoord board coord
            case maybeCoord of 
                Just c ->
                    let 
                        square =
                            Board.get board c
                    in 
                        return . attackCheck player $ square
                Nothing ->
                    return False



-- MOVES

                
getPieceFromSquare :: Square -> Maybe (Player,Piece)
getPieceFromSquare Blank = 
    Nothing
getPieceFromSquare (HasPiece player piece) =
    Just (player,piece)


getPiece :: (HasGame m) => (Int, Int) -> m (Maybe (Player,Piece))
getPiece pos = 
    do  (_, board) <- State.get
        return . (getPieceFromSquare <=< Board.maybeGet board) $ pos


makeMoveSet :: (HasGame m) => (Int,Int) -> Player -> Piece -> m (Set Board.Coord)
makeMoveSet pos _ Knight =
    do  (_, board) <- State.get
        moves <- Piece.knightMoves pos
        return . Board.coordSet board $ moves
makeMoveSet pos player (Pawn hasMoved) =
    do  (_, board) <- State.get
        case player of 
            Black ->
                do  moves <- Piece.pawnMoves hasMoved Piece.South pos
                    return . Board.coordSet board $ moves
            White -> 
                do  moves <- Piece.pawnMoves hasMoved Piece.North pos
                    return . Board.coordSet board $ moves
            

moveCheck :: HasGame m => (Int, Int) -> (Int, Int) -> m Bool
moveCheck start destination =
    do  (player, board) <- State.get
        maybePiece <- getPiece start 
        let destinationSet =
                Board.coordSet board (Set.singleton destination)

        case maybePiece of 
            Just (owner, piece) ->
                    do  moveSet <- makeMoveSet start player piece
                        return $ destinationSet `Set.isSubsetOf` moveSet

            Nothing ->
                return False
        
        
initialize :: Board Square
initialize = 
    undefined

        
