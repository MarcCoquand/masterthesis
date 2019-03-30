module Environment
    ( Environment
    , currentPlayer
    , handler
    , board
    , make
    , Environment.init
    ) where

import           Board      (Board)
import qualified Board
import           Coord      (Coord)
import qualified Coord
import           Data.Maybe (fromJust)
import           Piece      (Piece)
import qualified Piece
import           Player     (Player)
import qualified Player
import           Square     (Square)
import qualified Square


-- | The constructor is unsafe because it can be feeded a handler that does not
-- match the player and board.
data Environment = UnsafeMakeEnvironment
    { currentPlayer :: Player
    , board         :: Board Square
    , handler       :: Coord.Handle
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
