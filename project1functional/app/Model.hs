module Model
    ( Model
    , currentPlayer
    , handler
    , moveSet
    , board
    , make
    , Model.init
    , update
    ) where

import           Board    (Board)
import qualified Board
import           Data.Set (Set)
import qualified MoveSet
import           Piece    (Piece)
import qualified Piece
import           Player   (Player)
import qualified Player
import           Square   (Square)
import qualified Square


-- The constructor is unsafe because it can be feeded a handler that does not
-- match the player and board. Thus by hiding it from the export list we are
-- sure people have to create the model via the safe *make* function.
data Model = UnsafeMakeModel
    { currentPlayer :: Player
    , board         :: Board Square
    , handler       :: MoveSet.Handle
    }


make :: Player -> Board Square -> Model
make player newBoard =
    UnsafeMakeModel
        { currentPlayer = player
        , board = newBoard
        , handler = Square.handler player newBoard
        }


init :: Model
init =
    make Player.White Square.initialBoard


update :: Model -> [(Board.Coord, Square)] -> Model
update mdl changes =
    let
        player =
            Player.next (currentPlayer mdl)

        nextBoard =
            Board.update (board mdl) changes
    in
        make player nextBoard


isLegalMove :: Model -> Board.Coord -> Board.Coord -> Piece -> Bool
isLegalMove mdl start destination piece =
    let
        allowedMoves =
            moveSet mdl start piece
    in
        Board.isLegalMove allowedMoves destination


moveSet
    :: Model
    -> Board.Coord
    -> Piece
    -> Set Board.Coord
moveSet mdl pos Piece.Knight =
    let
        moves =
            MoveSet.knight (Model.handler mdl) (Board.extractCoord pos)
    in
        Board.coordSet (Model.board mdl) moves
moveSet mdl pos (Piece.Pawn hasMoved) =
        case Model.currentPlayer mdl of
            Player.Black ->
                let
                    moves =
                        MoveSet.pawn
                            (Model.handler mdl)
                            hasMoved
                            MoveSet.South
                            (Board.extractCoord pos)
                in
                    Board.coordSet (Model.board mdl) moves

            Player.White ->
                let
                    moves =
                        MoveSet.pawn
                            (Model.handler mdl)
                            hasMoved
                            MoveSet.North
                            (Board.extractCoord pos)
                in
                    Board.coordSet (Model.board mdl) moves

