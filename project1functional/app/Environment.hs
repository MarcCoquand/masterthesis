module Environment
    ( Environment
    , currentPlayer
    , handler
    , moveSet
    , board
    , make
    , Environment.init
    , applyChanges
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
-- sure people have to create the environment via the safe *make* function.
data Environment = UnsafeMakeEnvironment
    { currentPlayer :: Player
    , board         :: Board Square
    , handler       :: MoveSet.Handle
    }


make :: Player -> Board Square -> Environment
make player newBoard =
    UnsafeMakeEnvironment
        { currentPlayer = player
        , board = newBoard
        , handler = Square.handler player newBoard
        }


init :: Environment
init =
    make Player.White Square.initialBoard


applyChanges :: Environment -> [(Board.Coord, Square)] -> Environment
applyChanges env changes =
    let
        player =
            Player.next (currentPlayer env)
        nextBoard =
            Board.update (board env) changes
    in
        make player nextBoard


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
        case Environment.currentPlayer env of
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

