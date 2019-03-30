module Environment
    ( Environment
    , currentPlayer
    , handler
    , moveSet
    , board
    , make
    , Environment.init
    ) where

import           Board      (Board)
import qualified Board
import           Data.Maybe (fromJust)
import           Data.Set   (Set)
import           MoveSet    (Coord)
import qualified MoveSet
import           Piece      (Piece)
import qualified Piece
import           Player     (Player)
import qualified Player
import           Square     (Square)
import qualified Square


-- | The constructor is unsafe because it can be feeded a handler that does not
-- match the player and board. Thus by hiding it from the export list we are
-- sure people can not use it wrong.
data Environment = UnsafeMakeEnvironment
    { currentPlayer :: Player
    , board         :: Board Square
    , handler       :: MoveSet.Handle
    }


make :: Player -> Board Square -> Environment
make player board =
    UnsafeMakeEnvironment
        { currentPlayer = player
        , board = board
        , handler = Square.handler player board
        }


init :: Environment
init =
    make Player.White Square.initialBoard


moveSet
    :: Environment
    -> Board.Coord
    -> Piece
    -> Set Board.Coord
moveSet env pos Piece.Knight =
    let
        moves =
            MoveSet.knight (Environment.handler env) (Board.extractCoord pos)
    in
        Board.coordSet (Environment.board env) moves
moveSet env pos (Piece.Pawn hasMoved) =
        case (Environment.currentPlayer env) of
            Player.Black ->
                let
                    moves =
                        MoveSet.pawn
                            (Environment.handler env)
                            hasMoved
                            MoveSet.South
                            (Board.extractCoord pos)
                in
                    Board.coordSet (Environment.board env) moves

            Player.White ->
                let
                    moves =
                        MoveSet.pawn
                            (Environment.handler env)
                            hasMoved
                            MoveSet.North
                            (Board.extractCoord pos)
                in
                    Board.coordSet (Environment.board env) moves

