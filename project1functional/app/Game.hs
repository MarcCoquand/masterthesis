{-
 - Game logic, turns e.t.c.
 - Future developers should replace String Data.Text to gain extra performance.
 -}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}
module Game where


import           Board                     (Board)
import qualified Board
import           Control.Monad             (guard)
import           Control.Monad.Reader      (MonadReader)
import qualified Control.Monad.Reader      as Reader
import           Control.Monad.State.Lazy  (MonadState)
import qualified Control.Monad.State.Lazy  as State
import           Control.Monad.Writer.Lazy (MonadWriter)
import qualified Control.Monad.Writer.Lazy as Writer
import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as Map
import           Data.Maybe                (fromJust, maybe)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Piece
import           Text.Read                 (readMaybe)



-- * TYPES


data Player
    = Black
    | White
    deriving (Eq)


data Piece
    = Pawn Bool -- ^ Has the pawn moved or not
    | Knight


data Square
    = Blank
    | IsPiece Player Piece


type HasGame m = (MonadState (Player, Board Square) m, Piece.HasCheck m)


data Error
    = InvalidStartCoord
    | InvalidDestinationCoord



-- * INSTANCES


instance Show Piece where
    show (Pawn _) = "P"
    show Knight   = "K"


instance Show Square where
    show Blank = "  "
    show (IsPiece White piece) =
        "w" ++ show piece
    show (IsPiece Black piece) =
        "b" ++ show piece


collisionCheck :: Square -> Bool
collisionCheck Blank =
    False
collisionCheck (IsPiece _ _) =
    True


attackCheck :: Player -> Square -> Bool
attackCheck player Blank =
    True
attackCheck player (IsPiece pieceOwner _) =
    player == pieceOwner


checkSquare :: (Square -> Bool) -> Board Square -> Board.Coord -> Bool
checkSquare check fromBoard = check . Board.get fromBoard


checkCoordinate :: Board Square -> (Square -> Bool) -> (Int, Int) -> Bool
checkCoordinate board withCheck coord =
  let
    maybeCoord =
        Board.makeCoord board coord
  in
    maybe True (checkSquare withCheck board) maybeCoord


instance (Monad m, MonadState (Player, Board Square) m) => Piece.HasCheck m where
    isCollision coord =
        do  (_, board) <- State.get
            -- move as much logic out as possible from effectful
            -- computations for simple unit tests
            return $ checkCoordinate board collisionCheck coord

    isIndomitable coord =
        do  (player, board) <- State.get
            return $ checkCoordinate board (attackCheck player) coord



-- * MOVE


getPiece :: Square -> Maybe (Player, Piece)
getPiece square =
    case square of
        Blank ->
            Nothing

        IsPiece player piece ->
            Just (player, piece)


getPieceFromBoard :: (HasGame m) => Board.Coord -> m (Maybe (Player, Piece))
getPieceFromBoard pos =
    do  (_, board) <- State.get
        return . getPiece $ Board.get board pos


makeMoveSet :: (HasGame m) => Board.Coord -> Piece -> m (Set Board.Coord)
makeMoveSet pos Knight =
    do  (_, board) <- State.get
        moves      <- Piece.knightMoves (Board.extractCoord pos)
        return . Board.coordSet board $ moves
makeMoveSet pos (Pawn hasMoved) =
    do  (player, board) <- State.get
        case player of
            Black ->
                do  moves <-
                        Piece.pawnMoves
                            hasMoved
                            Piece.South
                            (Board.extractCoord pos)
                    return . Board.coordSet board $ moves

            White ->
                do  moves <- Piece.pawnMoves hasMoved Piece.North (Board.extractCoord pos)
                    return . Board.coordSet board $ moves


updatePieceInfo :: Piece -> Piece
updatePieceInfo Knight =
    Knight
updatePieceInfo (Pawn _) =
    Pawn True


movePieceIfAllowed
  :: Set Board.Coord
  -> Player
  -> Piece
  -> Board.Coord
  -> Board.Coord
  -> [(Board.Coord, Square)]
movePieceIfAllowed moveSet player piece start destination =
  if Set.member destination moveSet
    then
      [(start, Blank), (destination, IsPiece player (updatePieceInfo piece))]
    else []


nextBoard
  :: HasGame m => Board.Coord -> Board.Coord -> m [(Board.Coord, Square)]
nextBoard start destination =
    do  (player, board) <- State.get
        maybePiece <- getPieceFromBoard start

        case maybePiece of
              Just (pieceOwner, piece) ->
                if pieceOwner /= player
                    then return []
                else
                    do  moveSet <- makeMoveSet start piece
                        return $ movePieceIfAllowed moveSet player piece start destination

              Nothing -> return []


nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black


move
  :: (HasGame m)
  => (Int, Int)
  -> (Int, Int)
  -> m (Either Error (Player, Board Square))
move start destination = do
    (player, board) <- State.get

  -- Validate coords
    let maybeDestinationCoord =
            Board.makeCoord board destination

    let maybeStartCoord =
            Board.makeCoord board destination

    case (maybeStartCoord, maybeDestinationCoord) of
        (Just startCoord, Just destinationCoord) ->
            do  result <- nextBoard startCoord destinationCoord
                let newBoard = Board.update board result
                return . Right $ (nextPlayer player, newBoard)

        (Nothing, _) ->
            return . Left $ InvalidStartCoord

        (_ , Nothing) ->
            return . Left $ InvalidDestinationCoord



-- * GAME


parseInput :: String -> Maybe ((Int, Int), (Int, Int))
parseInput = readMaybe


executeMove
  :: (HasGame m, MonadWriter String m, MonadReader String m)
  => ((Int, Int), (Int, Int))
  -> m ()
executeMove (start, destination) = do
  maybeUpdated <- move start destination
  case maybeUpdated of
    Right updated -> do
      Writer.tell "Move successful"
      State.put updated
    Left InvalidStartCoord       -> Writer.tell "Invalid start square."
    Left InvalidDestinationCoord -> Writer.tell "Invalid destination square."


turn :: (HasGame m, MonadWriter String m, MonadReader String m) => m ()
turn =
  let throwInvalidMove =
        Writer.tell "Invalid move, format must be ((X,Y),(X,Y))"
  in  do
        input <- Reader.ask
        maybe throwInvalidMove executeMove (parseInput input)


initialize :: Board Square
initialize =
  let wP = IsPiece White (Pawn False)
      bP = IsPiece Black (Pawn False)
      wK = IsPiece White Knight
      bK = IsPiece Black Knight
      bl = Blank
  in
      -- fromJust is ok here as we want the program to crash if the board is
      -- not constructed correctly.
      fromJust . Board.construct $
      [ wP, wP, wK, wP, wP, wK, wP, wP
      , wP, wP, wP, wP, wP, wP, wP, wP
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bP, bP, bP, bP, bP, bP, bP, bP
      , bP, bP, bK, bP, bP, bK, bP, bP
      ]



