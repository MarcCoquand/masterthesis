module App where

import           Data.Maybe            (fromMaybe)
import           Navigator             (Navigator)
import qualified Navigator
import qualified System.Console.Pretty as Pretty
import qualified System.IO             as IO
import           Todo                  (Todo)
import qualified Todo


type TodoList =
    Navigator Todo


type OutputMessage = Maybe String


data Model
    = Is OutputMessage TodoList
    | Empty OutputMessage


data Msg
    = GotData TodoList
    | GotEmpty
    | Add Todo
    | MoveUp
    | MoveDown
    | Delete
    | Update String
    | Toggle


update :: Msg -> Model -> Model
update msg mdl =
    case (msg, mdl) of
        (GotData todoList, _) ->
            Is (Just "Loaded data") todoList

        (GotEmpty, _) ->
            Empty Nothing

        (Add todo, Empty _) ->
            Is (Just "Added item") (Navigator.init todo)
        (Add todo, Is _ todoList) ->
            Is (Just "Added item") (Navigator.add todoList todo)

        (MoveUp, Is output todoList) ->
            Is output (Navigator.up todoList)

        (MoveDown, Is output todoList) ->
            Is output (Navigator.down todoList)

        (Delete, Is _ todoList) ->
            case (Navigator.delete todoList) of
                Just updatedList ->
                    Is (Just "Deleted") updatedList
                Nothing ->
                    Empty Nothing

        (Update description, Is _ todoList) ->
            let
                item =
                    Todo.update (Navigator.get todoList) description
            in
                Is (Just "Updated item!") (Navigator.update todoList item)

        (Toggle, Is _ todoList) ->
            let
                toggled =
                    Todo.toggle . Navigator.get $ todoList
            in
                Is
                    (Just "Marked as complete")
                    (Navigator.update todoList toggled)

        (_, Empty _) ->
            Empty (Just "You need to add an item to do that")




makeTodo :: IO Todo
makeTodo =
    do  putStrLn "Enter description:"
        description <- getLine
        return (Todo.new description)


prompt :: Model -> Bool -> String
prompt (Empty maybeMessage) _ =
    fromMaybe "Your todo list is empty!" maybeMessage
prompt (Is message todoList) colorPrint =
    if colorPrint then
        fromMaybe "\n" (fmap (++ "\n") message) ++
        unlines
            [ unlines (fmap show . Navigator.above $ todoList)
            ++ Pretty.color Pretty.Blue (show (Navigator.get todoList))
            , unlines (fmap show . Navigator.below $ todoList)
            ]
    else
        fromMaybe "\n" (fmap (++ "\n") message) ++
        unlines
            [ unlines (fmap show . Navigator.above $ todoList)
            , show (Navigator.get todoList)
            , unlines (fmap show . Navigator.below $ todoList)
            ]


repl :: Bool -> Model -> IO ()
repl withPretty model =
    do  putStr "\ESC[2J"
        putStrLn (prompt model withPretty)
        input <- getChar
        case input of
            'a' ->
                do  IO.hSetBuffering IO.stdin IO.LineBuffering
                    IO.hSetEcho IO.stdin True
                    todo <- makeTodo
                    IO.hSetBuffering IO.stdin IO.NoBuffering
                    IO.hSetEcho IO.stdin False
                    repl withPretty (update (Add todo) model)
            'x' ->
                repl withPretty (update Toggle model)
            'k' ->
                repl withPretty (update MoveUp model)
            'j' ->
                repl withPretty (update MoveDown model)
            'd' ->
                repl withPretty (update Delete model)
            _ ->
                repl withPretty model




init :: Model
init =
    Empty Nothing


start :: IO ()
start =
    do  -- Read input right away
        IO.hSetBuffering IO.stdin IO.NoBuffering
        -- Hide input character
        IO.hSetEcho IO.stdin False
        hasColoredOutput <- Pretty.supportsPretty
        repl hasColoredOutput App.init
        undefined

