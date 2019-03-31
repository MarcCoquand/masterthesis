{-
 - Game logic, turns e.t.c.
 - Future developers should replace String Data.Text to gain extra performance.
 -}
module Game where


import qualified Board
import           Data.Function ((&))
import           Environment   (Environment)
import qualified Environment
import qualified Piece
import           Square        (Square)
import qualified Square
import           Text.Read     (readMaybe)



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
        "Move is not allowed."
    show BlankSpace =
        "Position is a blank space."


type Move = ((Int,Int),(Int,Int))


type State = (String, Environment)


getOwnedPiece
    :: Environment
    -> Board.Coord
    -> Either Error Square
getOwnedPiece env coord =
    case Board.get (Environment.board env) coord of
        Square.IsPiece pieceOwner piece ->
            if (pieceOwner == Environment.currentPlayer env) then
                Right (Square.IsPiece pieceOwner piece)
            else
                Left NotOwnedByPlayer

        Square.Blank ->
            Left BlankSpace


boardChanges
    :: Environment
    -> Square
    -> Board.Coord
    -> Board.Coord
    -> Either Error [(Board.Coord, Square)]
boardChanges env (Square.IsPiece player piece) startCoord destination =
    let
        moveSet =
            Environment.moveSet env startCoord piece
    in
        if Square.isLegalMove moveSet destination then
            Right
                [ (startCoord, Square.Blank)
                , (destination, Square.IsPiece player (Piece.update piece))
                ]
        else
            Left IllegalMove
boardChanges _ Square.Blank _ _ =
    Left BlankSpace


getWithError :: Environment -> (Int,Int) -> Error -> Either Error Board.Coord
getWithError env maybeCoord throwError =
    case Board.makeCoord (Environment.board env) maybeCoord of
        Just coord ->
            Right coord
        Nothing ->
            Left throwError


parseInput :: String -> Either Error Move
parseInput str =
    let
        input =
            readMaybe str
    in
        case input of
            Just correctInput ->
                Right correctInput

            Nothing ->
                Left BadParse


step :: State -> Either Error Environment
step (command, env) =
    do  (inputStart, inputDestination) <-
            parseInput command
        startCoord <-
            getWithError env inputStart InvalidStart
        destination <-
            getWithError env inputDestination InvalidDestination
        piece <-
            getOwnedPiece env startCoord
        changes <-
            boardChanges env piece startCoord destination
        changes
            & Environment.applyChanges env
            & return


data Result
    = Finished
    | Next State


-- | Returns Nothing if game has ended
runMove :: State -> Result
runMove state@(_, oldEnv) =
    case step state of
        -- TODO: Add win condition
        Right newEnv ->
            Next ("Move successful", newEnv)

        Left invalid ->
            Next (show invalid, oldEnv)


prompt :: State -> String
prompt (message, env) =
    show (Environment.board env)
        ++ "\n"
        ++ message
        ++ "\n"
        ++ "Current player is: "
        ++ show (Environment.currentPlayer env)


interact :: State -> IO Result
interact state@(_, env) =
    do  putStrLn (prompt state)
        input <- getLine
        return (runMove (input, env))


loop :: State -> IO ()
loop current =
    do  result <- Game.interact current
        case result of
            Next state ->
                loop state

            Finished ->
                putStrLn "Game ended"


start :: IO ()
start =
    loop
        ( "Welcome to chess! Make a move by typing\n"
            ++ "> \"((startX, startY),(endX,endY))\""
        , Environment.init)

