{-
 - Game logic, turns e.t.c.
 - Future developers should replace String Data.Text to gain extra performance.
 -}
module Game where


import           Board               (Board)
import qualified Board
import           Control.Monad.Extra (iterateM)
import           Data.Function       ((&))
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (fromJust, maybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Environment         (Environment)
import qualified Environment
import qualified MoveSet
import           Piece               (Piece)
import qualified Piece
import           Player              (Player)
import qualified Player
import           Square              (Square)
import qualified Square
import           Text.Read           (readMaybe)



data Error
    = InvalidStart
    | InvalidDestination
    | BadParse
    | NotOwnedByPlayer
    | IllegalMove
    | BlankSpace


instance Show Error where
    show InvalidStart =
        "Invalid start square."
    show InvalidDestination =
        "Invalid destination square."
    show BadParse =
        "Parse error. Format must be ((start x, start y), (end x, end y))"
    show NotOwnedByPlayer =
        "Piece is not owned by you."
    show IllegalMove =
        "Move is not allowed, could be a collision"
    show BlankSpace =
        "Position is a blank space."


type Move = ((Int,Int),(Int,Int))


type State = (String, Environment)


getOwnedPiece
    :: Environment
    -> Board.Coord
    -> Either Error (Player, Piece)
getOwnedPiece env coord =
    case Board.get (Environment.board env) coord of
        Square.IsPiece pieceOwner piece ->
            if (pieceOwner == Environment.currentPlayer env) then
                return (pieceOwner, piece)
            else
                Left NotOwnedByPlayer
        _ ->
            Left NotOwnedByPlayer


boardChanges
    :: Environment
    -> (Player, Piece)
    -> Board.Coord
    -> Board.Coord
    -> Either Error [(Board.Coord, Square)]
boardChanges env (player,piece) start destination =
    let
        moveSet =
            Environment.moveSet env start piece

        isLegal =
            Square.isLegalMove moveSet destination
    in
        if isLegal then
            Right
                [ (start, Square.Blank)
                , (destination, Square.IsPiece player (Piece.update piece))
                ]
        else
            Left IllegalMove


getStart :: Environment -> (Int,Int) -> Either Error Board.Coord
getStart env start =
    case Board.makeCoord (Environment.board env) start of
        Just coord ->
            Right coord
        Nothing ->
            Left InvalidStart


getDestination :: Environment -> (Int,Int) -> Either Error Board.Coord
getDestination env destination =
    case Board.makeCoord (Environment.board env) destination of
        Just coord ->
            Right coord
        Nothing ->
            Left InvalidDestination


applyChanges :: Environment -> [(Board.Coord, Square)] -> (Player, Board Square)
applyChanges env changes =
    ( Player.next (Environment.currentPlayer env)
    , Board.update (Environment.board env) changes
    )


move :: Environment -> Move -> Either Error (Player, Board Square)
move env (inputStart, inputDestination) =
    do  start <- getStart env inputStart
        destination <- getDestination env inputDestination
        piece <- getOwnedPiece env start
        changes <- boardChanges env piece start destination
        changes
            & applyChanges env
            & return


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
                Left BadParse


newState :: Environment -> String -> Either Error (Player, Board Square)
newState env command =
    do  moves <- parseInput command
        move env moves


data Result
    = Finished
    | Next State


-- | Returns Nothing if game has ended
runMove :: State -> Result
runMove (command, oldEnv) =
    case newState oldEnv command of
        -- TODO: Add win condition that returns result
        Right (player, board) ->
            Next ("Move successful", Environment.make player board)

        Left error ->
            Next (show error, oldEnv)



step :: State -> IO Result
step (message, env) =
    do  putStrLn . show $ Environment.board env
        putStrLn $ "\n" ++ message ++ "\n"
        putStrLn $ "Current player is: "
            ++ (show . Environment.currentPlayer $ env)
        input <- getLine
        return . runMove $ (input, env)


loop :: State -> IO ()
loop current =
    do  result <- step current
        case result of
            Next state ->
                loop state

            Finished ->
                putStrLn "Game ended"


run :: IO ()
run =
    loop ("Welcome to chess!", Environment.init)

