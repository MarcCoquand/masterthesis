{-
 - Game logic, turns e.t.c.
 - Future developers should replace String Data.Text to gain extra performance.
 -}
module Game where


import qualified Board
import           Model     (Model)
import qualified Model
import qualified Piece
import           Square    (Square)
import qualified Square
import           Text.Read (readMaybe)


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


-- Start -> Destination
type Move = ((Int,Int),(Int,Int))


type Message = String


type State = (Message, Model)


getPiece
    :: Model
    -> Board.Coord
    -> Either Error Square
getPiece mdl coord =
    case Board.get (Model.board mdl) coord of
        Square.IsPiece pieceOwner piece ->
            if (pieceOwner == Model.currentPlayer mdl) then
                Right (Square.IsPiece pieceOwner piece)
            else
                Left NotOwnedByPlayer

        Square.Blank ->
            Left BlankSpace


move
    :: Model
    -> Board.Coord
    -> Board.Coord
    -> Either Error [(Board.Coord, Square)]
move mdl start destination =
    do  (Square.IsPiece player piece) <- getPiece mdl start

        if Model.isLegalMove moveSet destination then
            Right
                [ (startCoord, Square.Blank)
                , (destination, Square.IsPiece player (Piece.update piece))
                ]
        else
            Left IllegalMove


getAt :: Model -> Error -> (Int,Int) -> Either Error Board.Coord
getAt mdl throwError maybeCoord =
    case Board.makeCoord (Model.board mdl) maybeCoord of
        Just coord ->
            Right coord
        Nothing ->
            Left throwError


getStart :: Model -> (Int,Int) -> Either Error Board.Coord
getStart mdl maybeCoord =
    getAt mdl InvalidStart maybeCoord


getDestination :: Model -> (Int,Int) -> Either Error Board.Coord
getDestination mdl maybeCoord =
    getAt mdl InvalidDestination maybeCoord


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


update :: State -> Either Error Model
update (command, mdl) =
    do  (requestedStart, requestedDestination) <-
            parseInput command
        start <-
            getStart mdl requestedStart
        destination <-
            getDestination mdl requestedDestination
        updatedCoordinates <-
            move mdl start destination
        return (Model.update mdl updatedCoordinates)


data Result
    = Finished
    | Next State


step :: State -> Result
step state@(_, mdl) =
    case update state of
        -- TODO: Add win condition
        Right newMdl ->
            Next ("Move successful", newMdl)

        Left invalid ->
            Next (show invalid, mdl)


-- | Evaluate the result of interact.
--  [@(State -> m ()@] A monadic loop with the new state
--  [@m ()@]           Function to run when game ends
evaluate :: Result -> (State -> m ()) -> m () -> m ()
evaluate result next end =
    case result of
        Next state ->
            next state

        Finished ->
            end


prompt :: State -> String
prompt (message, mdl) =
    unlines
        [ show (Model.board mdl)
        , ""
        , message
        , ""
        , "Current player is: " ++ show (Model.currentPlayer mdl)
        ]


interact :: State -> IO Result
interact state@(_, mdl) =
    do  putStrLn (prompt state)
        input <- getLine
        return (step (input, mdl))


loop :: State -> IO ()
loop current =
    let
        end =
            putStrLn "Game ended"
    in
        do  result <- Game.interact current
            evaluate result loop end


run :: IO ()
run =
    let
        welcome =
            "Welcome to chess! Make a move by typing \
            \\"((startX, startY),(endX,endY))\""
    in
        loop (welcome, Model.init)

