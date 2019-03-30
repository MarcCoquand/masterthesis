module Square where

import           Board      (Board)
import qualified Board
import           Coord      (Coord)
import qualified Coord
import           Data.Maybe (fromJust)
import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Piece      (Piece)
import qualified Piece
import           Player     (Player)
import qualified Player


data Square
    = Blank
    | IsPiece Player Piece


instance Show Square where
    show Blank = "  "
    show (IsPiece Player.White piece) =
        "w" ++ show piece
    show (IsPiece Player.Black piece) =
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


check :: Board Square -> (Square -> Bool) -> (Int, Int) -> Bool
check board checkSquare coord =
  let
      maybeCoord =
          Board.makeCoord board coord
  in
      case maybeCoord of
        Just coord ->
            checkSquare $ Board.get board coord

        Nothing ->
            True


getPiece :: Square -> Maybe (Player, Piece)
getPiece square =
    case square of
        Blank ->
            Nothing

        IsPiece player piece ->
            Just (player, piece)


moveIfAllowed
  :: Set Board.Coord
  -> Player
  -> Piece
  -> Board.Coord
  -> Board.Coord
  -> [(Board.Coord, Square)]
moveIfAllowed moveSet player piece start destination =
  if Set.member destination moveSet
    then
      [(start, Blank), (destination, IsPiece player (Piece.update piece))]
    else []


handler :: Player -> Board Square -> Coord.Handle
handler player board =
        Coord.MakeHandle
            { Coord.isCollision = \coord ->
                -- move as much logic out as possible from effectful
                -- computations for simple unit tests
                check board collisionCheck coord

            , Coord.isIndomitable = \coord ->
                check board (attackCheck player) coord
            }


initialBoard :: Board Square
initialBoard =
  let wP = Square.IsPiece Player.White (Piece.Pawn False)
      bP = Square.IsPiece Player.Black (Piece.Pawn False)
      wK = Square.IsPiece Player.White Piece.Knight
      bK = Square.IsPiece Player.Black Piece.Knight
      bl = Square.Blank
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

