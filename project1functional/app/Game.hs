{-
 - Game logic, turns e.t.c.
 - Future developers should replace String Data.Text to gain extra performance.
 -}
module Game where


import           Board               (Board)
import qualified Board
import           Control.Monad.Extra (iterateM)
import qualified Coord
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (fromJust, maybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Environment         (Environment)
import qualified Environment
import           Piece               (Piece)
import qualified Piece
import           Player              (Player)
import qualified Player
import           Square              (Square)
import qualified Square
import           Text.Read           (readMaybe)




data Error
    = InvalidStartCoord
    | InvalidDestinationCoord
    | ParseError
    | NotOwnedByPlayer
    | NotLegalMove
    | IsBlankSpace
    deriving (Show)



type Move = ((Int,Int),(Int,Int))


type State = (String, Environment)


getPieceFromBoard
    :: Board Square
    -> Board.Coord
    -> Maybe (Player, Piece)
getPieceFromBoard board pos =
    Square.getPiece $ Board.get board pos


makeMoveSet
    :: Environment
    -> Board.Coord
    -> Piece
    -> Set Board.Coord
makeMoveSet env pos Piece.Knight =
    let
        moves =
            Coord.knightMoves (Environment.handler env) (Board.extractCoord pos)
    in
        Board.coordSet (Environment.board env) $ moves
makeMoveSet env pos (Piece.Pawn hasMoved) =
        case (Environment.currentPlayer env) of
            Player.Black ->
                let
                    moves =
                        Coord.pawnMoves
                            (Environment.handler env)
                            hasMoved
                            Coord.South
                            (Board.extractCoord pos)
                in
                    Board.coordSet (Environment.board env) $ moves

            Player.White ->
                let
                    moves =
                        Coord.pawnMoves
                            (Environment.handler env)
                            hasMoved
                            Coord.North
                            (Board.extractCoord pos)
                in
                    Board.coordSet (Environment.board env) $ moves


nextBoard
  :: Environment
  -> Board.Coord
  -> Board.Coord
  -> Either Error [(Board.Coord, Square)]
nextBoard env start destination =
    let
        maybePiece =
            getPieceFromBoard (Environment.board env) start
    in
        case maybePiece of
          Just (pieceOwner, piece) ->
            if pieceOwner /= (Environment.currentPlayer env)
                then Left NotOwnedByPlayer
            else
                let
                    moveSet =
                        makeMoveSet env start piece
                in
                    Right $ Square.moveIfAllowed
                        moveSet
                        (Environment.currentPlayer env)
                        piece
                        start
                        destination

          Nothing -> Left IsBlankSpace


move
  :: Environment
  -> Move
  -> Either Error (Player, Board Square)
move env (start,destination) =
    -- Validate coords
    let
        maybeDestinationCoord =
            Board.makeCoord (Environment.board env) destination

        maybeStartCoord =
            Board.makeCoord (Environment.board env) start

    in
        case (maybeStartCoord, maybeDestinationCoord) of
            (Just startCoord, Just destinationCoord) ->
                    do  result <- nextBoard env startCoord destinationCoord
                        let newBoard =
                                Board.update (Environment.board env) result
                        return (Player.next (Environment.currentPlayer env), newBoard)

            (Nothing, _) ->
                Left InvalidStartCoord

            (_ , Nothing) ->
                Left InvalidDestinationCoord



parseInput :: String -> Either Error Move
parseInput str =
    let
        input =
            readMaybe str
    in
        case input of
            Just input ->
                Right input
            Nothing ->
                Left ParseError


attemptMove :: Environment -> String -> Either Error (Player, Board Square)
attemptMove env str =
    do  moves <- parseInput str
        move env moves


runMove :: State -> Maybe State
runMove (str,env) =
    let updated =
            attemptMove env str

    in
        case updated of
            Right (updatedPlayer, updatedBoard) ->
                Just ("Move successful", Environment.make updatedPlayer updatedBoard)

            Left InvalidStartCoord ->
                Just ("Invalid start square.", env)

            Left InvalidDestinationCoord ->
                Just ("Invalid destination square.", env)

            Left ParseError ->
                Just ("Parse error.", env)

            Left NotOwnedByPlayer ->
                Just ("Piece is not owned by you.", env)

            Left IsBlankSpace ->
                Just ("Position is a blank space.", env)

            -- To be replaced with win condition (which should not be an error
            _ ->
                Nothing


step :: State -> IO (Maybe State)
step (str, env) =
    do  putStrLn . show $ Environment.board env
        putStrLn $ "\n" ++ str ++ "\n"
        putStrLn $ "Current player is: " ++ (show . Environment.currentPlayer $ env)
        putStr "> "
        next <- getLine
        return . runMove $ (next, env)


loop :: State -> IO ()
loop currentState =
    do  result <- step currentState
        case result of
            Just nextState ->
                loop nextState

            Nothing ->
                putStrLn "Game ended"

run :: IO ()
run =
    loop ("Welcome to chess!", Environment.init)

