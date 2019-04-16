module Model
    ( Model
    , currentPlayer
    , handler
    , moveSet
    , board
    , make
    , Model.init
    , update
    , getCoordinate
    , move
    , getOwnedPiece
    , pieceIsOwnedByPlayer
    , prompt
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
-- match the player and board. By hiding it from the export list we are
-- sure people have to create the model via the safe *make* function that we can
-- ensure is safe.
data Model = UnsafeMakeModel
    { currentPlayer :: Player
    , board         :: Board Square
    , handler       :: MoveSet.Handle
    }



-- * CREATION


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



-- * MOVE PIECE


isLegalMove :: Model -> BoardPiece -> Board.Coord -> Bool
isLegalMove mdl piece destination =
    let
        allowedMoves =
            moveSet mdl piece
    in
        Board.isLegalMove allowedMoves destination


getCoordinate :: Model -> (Int,Int) -> Maybe Board.Coord
getCoordinate mdl maybeCoord =
    Board.makeCoord (Model.board mdl) maybeCoord


moveSet
    :: Model
    -> BoardPiece
    -> Set Board.Coord
moveSet mdl (BoardPiece (pos, _, Piece.Knight)) =
    let
        moves =
            MoveSet.knight (Model.handler mdl) (Board.extractCoord pos)
    in
        -- Removes all out of bounds options
        Board.coordSet (Model.board mdl) moves
moveSet mdl (BoardPiece (pos , _, (Piece.Pawn hasMoved))) =
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


newtype BoardPiece = BoardPiece (Board.Coord, Player, Piece)


pieceIsOwnedByPlayer :: Model -> BoardPiece -> Bool
pieceIsOwnedByPlayer model (BoardPiece (_, pieceOwner, _)) =
    pieceOwner == Model.currentPlayer model


getOwnedPiece
    :: Model
    -> Board.Coord
    -> Maybe BoardPiece
getOwnedPiece mdl coord =
    case Board.get (Model.board mdl) coord of
        Square.IsPiece pieceOwner piece ->
            if pieceOwner == (currentPlayer mdl) then
                Just (BoardPiece (coord,pieceOwner,piece))
            else
                Nothing
        Square.Blank ->
            Nothing


-- | Move the piece IFF destination is in the set of allowed moves
move
    :: Model
    -> BoardPiece
    -> Board.Coord
    -> Maybe [(Board.Coord, Square)]
move mdl boardpiece@(BoardPiece (startCoord,player,piece)) destination =
    if isLegalMove mdl boardpiece destination then
        Just
            [ (startCoord, Square.Blank)
            , (destination, Square.IsPiece player (Piece.update piece))
            ]
    else
        Nothing


prompt :: Model -> String -> String
prompt model message =
    unlines
        [ show (board model)
        , ""
        , message
        , ""
        , "Current player is: " ++ show (currentPlayer model)
        ]

