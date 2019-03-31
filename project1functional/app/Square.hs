module Square where

import           Board      (Board)
import qualified Board
import           Data.Maybe (fromJust)
import           Data.Set   (Set)
import qualified Data.Set   as Set
import qualified MoveSet
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


isCollision :: Square -> Bool
isCollision Blank =
    False
isCollision (IsPiece _ _) =
    True


isIndomitable :: Player -> Square -> Bool
isIndomitable _ Blank =
    True
isIndomitable player (IsPiece pieceOwner _) =
    player == pieceOwner


check :: Board Square -> (Square -> Bool) -> (Int, Int) -> Bool
check board checkSquare coord =
  let
      maybeCoord =
          Board.makeCoord board coord
  in
      case maybeCoord of
        Just success ->
            checkSquare $ Board.get board success

        Nothing ->
            True


isLegalMove
    :: Set Board.Coord
    -> Board.Coord
    -> Bool
isLegalMove moveSet destination =
    Set.member destination moveSet


handler :: Player -> Board Square -> MoveSet.Handle
handler player board =
        MoveSet.MakeHandle
            { MoveSet.isCollision = \coord ->
                check board isCollision coord
            , MoveSet.isIndomitable = \coord ->
                check board (isIndomitable player) coord
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

