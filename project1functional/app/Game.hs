{-
 - In the future one could replace String with Data.Text to gain extra
 - performance.
 -}
module Game where


import           Data.Function ((&))
import           Model         (Model)
import qualified Model
import           Text.Read     (readMaybe)


type State = (String, Model)


data Result
    = Finished
    | Next State


loop :: State -> IO ()
loop (message, model) =
    do  result <-
            step model message
        case result of
            Next state ->
                loop state
            Finished ->
                finish


finish :: IO ()
finish =
    putStrLn "Game ended"


step :: Model -> String -> IO Result
step model message =
    do  putStrLn (Model.prompt model message)
        command <- getLine
        return (update model command)


update :: Model -> String -> Result
update model command =
    case runCommand model command of
        -- TODO: Add win condition
        Right newModel ->
            Next ("Move successful", newModel)

        Left invalidInput ->
            Next (showMistake invalidInput, model)


data InvalidInput
    = Start
    | Destination
    | Format
    | PieceChoice
    | MoveRange


-- TODO: parse input a...h as 1...8
runCommand :: Model -> String -> Either InvalidInput Model
runCommand model input =
    do  (requestedStart, requestedDestination) <-
            readMaybe input
                & catchInvalid Format
        start <-
            Model.getCoordinate model requestedStart
                & catchInvalid Start
        destination <-
            Model.getCoordinate model requestedDestination
                & catchInvalid Destination
        piece <-
            Model.getOwnedPiece model start
                & catchInvalid PieceChoice
        changedSquares <-
            Model.move model piece destination
                & catchInvalid MoveRange

        return (Model.update model changedSquares)


-- We do not instance Invalid with Show because an instance of Show should follow
-- the law (read . show) x == x.
showMistake :: InvalidInput -> String
showMistake mistake =
    case mistake of
        Start ->
            "Invalid start square."
        Destination ->
            "Invalid destination square."
        Format ->
            "Parse error. Format must be ((start x, start y), (end x, end y))"
        PieceChoice ->
            "Square does not contain a piece owned by you."
        MoveRange ->
            "The piece is not allowed to move to the given coordinate."


catchInvalid :: InvalidInput -> Maybe a -> Either InvalidInput a
catchInvalid invalid result =
    case result of
        Just value ->
            Right value
        Nothing ->
            Left invalid

