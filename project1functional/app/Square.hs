module Square where

import           Board         (Board)
import qualified Board
import           Data.Function ((&))
import           Data.Maybe    (fromJust)
import qualified MoveSet
import           Piece         (Piece)
import qualified Piece
import           Player        (Player)
import qualified Player


data Square
    = Blank
    | IsPiece Player Piece
    deriving (Eq)


instance Show Square where
    show Blank =
        "  "
    show (IsPiece Player.White piece) =
        "w" ++ show piece
    show (IsPiece Player.Black piece) =
        "b" ++ show piece



-- * MOVE VALIDATION


handler :: Player -> Board Square -> MoveSet.Handle
handler player board =
    MoveSet.MakeHandle
        { MoveSet.isCollision =
            \coord ->
                check board isCollision coord
        , MoveSet.isIndomitable =
            \coord ->
                check board (isIndomitable player) coord
        }



check :: Board Square -> (Square -> Bool) -> (Int, Int) -> Bool
check board checkSquare toCheck =
  let
      maybeCoord =
          Board.makeCoord board toCheck
  in
      case maybeCoord of
        Just inboundCoord ->
            checkSquare (Board.get board inboundCoord)

        Nothing ->
            True


isCollision :: Square -> Bool
isCollision square =
    square /= Blank


-- Can the user NOT attack this board
isIndomitable :: Player -> Square -> Bool
isIndomitable player square =
    case square of
        IsPiece pieceOwner _ ->
            player == pieceOwner

        Blank ->
            True



-- * BOARD


initialBoard :: Board Square
initialBoard =
  let
      wP = Square.IsPiece Player.White (Piece.Pawn False)
      bP = Square.IsPiece Player.Black (Piece.Pawn False)
      wK = Square.IsPiece Player.White Piece.Knight
      bK = Square.IsPiece Player.Black Piece.Knight
      bl = Square.Blank
  in
      -- fromJust is ok here as we want the program to crash if the board is
      -- not constructed correctly.
      [ wP, wP, wK, wP, wP, wK, wP, wP
      , wP, wP, wP, wP, wP, wP, wP, wP
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bP, bP, bP, bP, bP, bP, bP, bP
      , bP, bP, bK, bP, bP, bK, bP, bP
      ]
          & Board.construct
          & fromJust
