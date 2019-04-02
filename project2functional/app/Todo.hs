module Todo where


data Todo =
    MakeTodo
    { description :: String
    , done        :: Bool
    }


formatDone :: Todo -> String
formatDone todo =
    if done todo then
        "[X]"
    else
        "[ ]"


instance Show Todo where
    show todo =
        unwords
            [ formatDone todo
            , description todo
            ]


toggle :: Todo -> Todo
toggle todo =
    MakeTodo { done = not (done todo), description = (description todo) }


update :: Todo -> String -> Todo
update todo newDesc =
    MakeTodo { description = newDesc, done = (done todo) }


new :: String -> Todo
new desc =
    MakeTodo
        { done = False
        , description = desc
        }

